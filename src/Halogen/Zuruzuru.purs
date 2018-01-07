module Halogen.Zuruzuru
  ( zuruzuru
  , Direction(..)
  , Helpers
  , Handle
  , Item
  , Query(..)
  , State
  , DragState
  , Keyed
  , Key
  , main
  ) where


import Prelude

import Control.Apply (lift2)
import Control.Extend (extend)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Event.Types (MouseEvent)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import Data.Array (deleteAt, filter, findIndex, findLastIndex, fromFoldable, insertAt, length, updateAt, (!!))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Traversal', _Just, preview, view, (%=), (+=), (.=), (?=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)

-- | An element with a key (string), pretty simple.
type Keyed = Tuple Key

-- | Items are kept track of with a string key, for Halogen's keyed elements.
type Key = String

-- | Query for the component.
data Query o e a
  -- | Reset all the items in this component
  = Reset (Array e) a
  -- | Add a component at this index (between 0 and the length of the existing
  -- | elements, inclusive)
  | Add Int a
  -- | Set the item at the corresponding key to that value.
  | Update String e a
  -- | Remove the item with this key
  | Remove Key a
  -- | Swap the items at the corresponding indices.
  | Swap Int Int a
  -- | Start dragging the item with corresponding key.
  | Dragging Key MouseEvent a
  -- | Used to communicate with the SlamData Drag API, for when the mouse moves
  -- | or releases.
  | Move Drag.DragEvent a
  -- | Finalize a drag, placing the dragged item at this index.
  | DragTo Int a
  -- | Raise an output through the component.
  | Output o a

-- | The state of the component.
type State e =
  { values :: Array (Keyed e)
  , dragging :: Maybe DragState
  , supply :: Int
  }

-- | State maintained while dragging. Together the numbers are used to
-- | translate the corresponding item (using a CSS transform).
type DragState =
  { -- | The key of the item being dragged
    key :: String
  -- | The offset from its original position, whence it was dragged
  , offset :: Number
  -- | The displacement of the mouse from the start of the drag
  , displacement :: Number
  }

fresh :: forall s m. MonadState s m => Lens' s Int -> m Int
fresh lens = H.gets (view lens) <* H.modify (lens (_+1))

_values :: forall e. Lens' (State e) (Array (Keyed e))
_values = prop (SProxy :: SProxy "values")

_dragging :: forall e. Lens' (State e) (Maybe DragState)
_dragging = prop (SProxy :: SProxy "dragging")

_dragKey :: forall e. Traversal' (State e) String
_dragKey = _dragging <<< _Just <<< prop (SProxy :: SProxy "key")

_offset :: forall e. Traversal' (State e) Number
_offset = _dragging <<< _Just <<< prop (SProxy :: SProxy "offset")

initialState :: Array ~> State
initialState vs = { values: addKeys vs, supply: length vs, dragging: Nothing }

addKeys :: forall e. Array e -> Array (Keyed e)
addKeys = mapWithIndex (Tuple <<< append "item" <<< show)

surroundMapWithIndices :: forall m a. Monoid m =>
  (Int -> m) ->
  (Int -> a -> m) ->
  Array a -> m
surroundMapWithIndices m f as = (_ <> m (length as)) $
  as # foldMapWithIndex \i a ->
    m i <> f i a

-- | The (opaque) queries to perform certain actions on an item.
type Helpers o q e =
  { -- | Swap this item with the previous one, if not the first
    prev :: Maybe q
  -- | Swap this item with the next one, if not the last
  , next :: Maybe q
  -- | Remove this item
  , remove :: q
  -- | Give this item a new value
  , set :: e -> q
  -- | Lift an output query through the component
  , output :: o -> q
  }

-- | The property for a handler. No in the `Helpers` record because records
-- | hate impredicativity.
type Handle q = forall r. HH.IProp ( onMouseDown :: MouseEvent | r ) q

-- | Information about an item stored in state.
type Item e =
  { -- | The unique key given to this item
    key :: String
  -- | The current index of this item
  , index :: Int
  -- | The current value of this item
  , value :: e
  }

-- | Whether the list is rendered vertically or horizontally. Horizontal has
-- | a (horizontal) scrollbar when needed.
data Direction = Vertical | Horizontal

-- | Render a `zuruzuru` component! Allows a list of components to be edited
-- | and rearranged.
-- |
-- | `zuruzuru dir default addBtn render1` takes
-- |   - A direction (Horizontal or Vertical) for the list
-- |   - A default value used for adding new items
-- |   - A way of (maybe) rendering a button to add items (in between each item
-- |     and at the start and end of the list)
-- |   - A way of (definitely) rendering each list item, given certain queries
-- |     (see `Helpers`), an `onMouseDown` property for the drag handle, and
-- |     information about the item (including its position, see `Item`)
zuruzuru :: forall m e o eff.
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  Direction ->
  -- | Default value
  e ->
  -- | Render a button for adding a component
  (forall q. q -> Maybe (HH.HTML Void q)) ->
  -- | Render an item given certain queries, an `onMouseDown` property for the
  -- | draggable handle, and information about the item.
  (forall q. Helpers o q e -> Handle q -> Item e -> HH.HTML Void q) ->
  H.Component HH.HTML (Query o e) (Array e) o m
zuruzuru dir default addBtn render1 =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Reset
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    label i = H.RefLabel ("zuruzuru-component" <> i) :: H.RefLabel

    item k props children = [ Tuple k (HH.div props children) ]
    adding i = join $ fromFoldable $ addBtn (Add i unit) <#> \b -> item ("add" <> show i) [] [ b ]
    dragStyle :: forall r i. Maybe DragState -> String -> HP.IProp r i
    dragStyle dragging key = HP.attr (H.AttrName "style") case dragging of
      Just { key: k, displacement, offset } | k == key ->
        "transform: translateY(" <> show (displacement - offset) <> "px)"
      _ -> ""

    render :: State e -> H.ComponentHTML (Query o e)
    render { values, dragging } = HK.div_ $ values #
      let top = length values - 1 in
      surroundMapWithIndices adding \i (Tuple k v) ->
        item k [ HP.ref (label k), dragStyle dragging k ]
          $ pure $ render1
            { prev: guard (i > 0) $> Swap i (i-1) unit
            , next: guard (i < top) $> Swap i (i+1) unit
            , remove: Remove k unit
            , set: Update k <@> unit
            , output: Output <@> unit
            } (HE.onMouseDown (HE.input (Dragging k))) { key: k, index: i, value: v }

    mid = _.top `lift2 (+)` _.bottom >>> (_ / 2.0)

    getPos :: Key -> MaybeT (H.ComponentDSL (State e) (Query o e) o m) Number
    getPos k = do
      e <- MaybeT $ H.getRef (label k)
      mid <$> H.liftEff (getBoundingClientRect (unsafeCoerce e))

    getPoses :: H.ComponentDSL (State e) (Query o e) o m (Array (Maybe Number))
    getPoses =
      H.gets (view _values) >>=
        traverse \(Tuple k _) ->
          runMaybeT $ getPos k

    fresh' = fresh (prop (SProxy :: SProxy "supply"))

    eval :: Query o e ~> H.ComponentDSL (State e) (Query o e) o m
    eval (Output o next) = next <$ do
      H.raise o
    eval (Reset values next) = next <$ do
      H.put (initialState values)
    eval (Update k v next) = next <$ do
      _values %= map (extend \(Tuple k' v') -> if k == k' then v else v')
    eval (Add i next) = next <$ do
      k <- append "item" <<< show <$> fresh'
      _values %= (fromMaybe <*> insertAt i (Tuple k default))
    eval (Remove k next) = next <$ do
      _values %= filter (fst >>> notEq k)
    eval (Swap i j next) = next <$ runMaybeT do
      values <- H.lift $ H.gets (view _values)
      a <- MaybeT $ pure (values !! i)
      b <- MaybeT $ pure (values !! j)
      v' <- MaybeT $ pure (updateAt i b values)
      v'' <- MaybeT $ pure (updateAt j a v')
      _values .= v''
    eval (DragTo i' next) = next <$ runMaybeT do
      dragging <- MaybeT $ H.gets $ view _dragging
      let k = dragging.key
      oldPos <- getPos k
      values <- H.gets (view _values)
      values' <- MaybeT $ pure do
        i <- findIndex (fst >>> eq k) values
        v <- values !! i
        deleteAt i values >>= insertAt i' v
      H.modify _ { values = values' }
      newPos <- getPos k
      _offset += (newPos - oldPos)
    eval (Dragging k e next) = next <$ runMaybeT do
      H.lift $ H.subscribe $ Drag.dragEventSource e \e' -> Just $ Move e' H.Listening
      _dragging ?= { key: k, displacement: 0.0, offset: 0.0 }
    eval (Move (Drag.Move e d) next) = next <$ do
      _dragging %= map _ { displacement = d.offsetY }
      poses <- getPoses
      runMaybeT do
        k <- MaybeT $ H.gets $ preview _dragKey
        values <- H.gets (view _values)
        i <- MaybeT $ pure $ findIndex (fst >>> eq k) values
        p <- getPos k
        let
          -- these will either default to `i` (already) or
          -- be the index that this should go to
          least = fromMaybe i $ poses # findIndex (maybe false (_ >= p))
          most = fromMaybe i $ poses # findLastIndex (maybe false (_ <= p))
        guard (least < most)
        let
          i'
            | most > i && least == i = most
            | least < i && most == i = least
            | otherwise = i
        guard (i /= i') -- redundant, but just to be safe
        H.lift $ eval (DragTo i' unit)
    eval (Move (Drag.Done e) next) = next <$ do
      _dragging .= Nothing

data DemoQuery a
  = Receive Void a

demo :: forall u v m eff.
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  H.Component HH.HTML DemoQuery u v m
demo =
  H.lifecycleParentComponent
    { initialState: const ["","",""]
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    btn :: forall q. Maybe q -> String -> HH.HTML Void q
    btn q t = HH.button [ HE.onClick (pure q), HP.disabled (isNothing q) ] [ HH.text t ]
    add :: forall q. q -> Maybe (HH.HTML Void q)
    add q = Just $ btn (Just q) "Add"
    com = zuruzuru Vertical mempty add \{ next, prev, remove, set } -> \handle ->
      \{ key: k, index: i, value: v } -> HH.div_
        [ btn prev "▲"
        , HH.button [ handle, HP.attr (H.AttrName "style") "pointer: move" ] [ HH.text "≡" ]
        , btn next "▼"
        , HH.text (" " <> show (i+1) <> ". ")
        , HH.input
          [ HP.value v, HE.onValueInput (Just <<< set) ]
        , btn (Just remove) "-"
        ]

    update = H.put >>> (_ *> inform)
    inform = do
      r <- H.get
      H.liftEff $ logShow r

    render :: Array String -> H.ParentHTML DemoQuery (Query Void String) Unit m
    render s = HH.div_ [HH.slot unit com s (HE.input Receive)]

    eval :: DemoQuery ~> H.ParentDSL (Array String) DemoQuery (Query Void String) Unit v m
    eval (Receive v a) = a <$ do
      H.liftEff $ log "Update"
      update (absurd v)

-- | Demo app.
main :: forall e. Eff ( avar :: AVAR, ref :: REF, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE | e ) Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit
