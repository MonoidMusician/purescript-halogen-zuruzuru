module Halogen.Zuruzuru
  ( zuruzuru
  , zuru
  , Direction(..)
  , Helpers
  , Handle
  , Item
  , Query(..)
  , Message(..)
  , Output
  , SimpleOutput
  , State
  , DragState
  , Keyed
  , Key
  , SimpleHTML
  , main
  ) where


import Prelude

import Control.Apply (lift2)
import Control.Comonad (extract)
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
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Array (deleteAt, filter, findIndex, findLastIndex, fromFoldable, insertAt, length, updateAt, (!!))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Traversal', _Just, preview, use, (%=), (+=), (.=), (?=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Halogen as H
import Halogen.Aff (awaitLoad, runHalogenAff, selectElement)
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
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
  -- | Update the item at the corresponding key, providing a new value from an old value.
  | Update String (e -> e) a
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

-- | The output message from the component.
data Message e
  = NewState (Array e)
  | Preview (Array e)
  | DragStart
  | DragEnd

-- | Either a message from the component or the out passed through the component.
type Output o e = Either (Message e) o

-- | No output will be passed through, `Void`
type SimpleOutput e = Output Void e

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
fresh lens = use lens <* H.modify (lens (_+1))

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
    prev :: Maybe (q Unit)
  -- | Swap this item with the next one, if not the last
  , next :: Maybe (q Unit)
  -- | Remove this item
  , remove :: q Unit
  -- | Give this item a new value
  , set :: e -> q Unit
  -- | Modify this item's value
  , modify :: (e -> e) -> q Unit
  -- | Lift an output query through the component
  , output :: o -> q Unit
  }

-- | The property for a handler. No in the `Helpers` record because records
-- | hate impredicativity.
type Handle q = forall r. H.IProp ( onMouseDown :: MouseEvent | r ) q

-- | Information about an item stored in state.
type Item e =
  { -- | The unique key given to this item
    key :: String
  -- | The current index of this item
  , index :: Int
  -- | The current value of this item
  , value :: e
  , dragged :: Boolean
  }

-- | Whether the list is rendered vertically or horizontally. Horizontal has
-- | a (horizontal) scrollbar when needed.
data Direction = Vertical | Horizontal

type SimpleHTML q m = H.ParentHTML q (Const Void) Void m

-- | `zuruzuru` minus the higher-order parent component junk.
zuru :: forall m e o eff.
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  Direction ->
  -- | Default value
  e ->
  -- | Render a button for adding a component
  (forall q. q Unit -> Maybe (SimpleHTML q m)) ->
  -- | Render an item given certain queries, an `onMouseDown` property for the
  -- | draggable handle, and information about the item.
  (forall q. Helpers o q e -> Handle q -> Item e -> SimpleHTML q m) ->
  H.Component HH.HTML (Query o e) (Array e) (Output o e) m
zuru = zuruzuru

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
zuruzuru :: forall m e o eff g p.
  Ord p =>
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  Direction ->
  -- | Default value
  e ->
  -- | Render a button for adding a component
  (forall q. q Unit -> Maybe (H.ParentHTML q g p m)) ->
  -- | Render an item given certain queries, an `onMouseDown` property for the
  -- | draggable handle, and information about the item.
  (forall q. Helpers o q e -> Handle q -> Item e -> H.ParentHTML q g p m) ->
  H.Component HH.HTML (Query o e) (Array e) (Output o e) m
zuruzuru dir default addBtn render1 =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Reset
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    label i = H.RefLabel ("zuruzuru-component" <> i) :: H.RefLabel

    itemStyle :: forall r i. String -> HP.IProp ( style :: String | r ) i
    itemStyle = HP.attr (H.AttrName "style") <<< (_ <> display) where
      display = case dir of
        Horizontal -> "; display: inline-block"
        Vertical -> "; display: block"

    overflow = "overflow: auto; -webkit-overflow-scrolling: touch; "

    topStyle :: forall r i. HP.IProp ( style :: String | r ) i
    topStyle = HP.attr (H.AttrName "style") $ overflow <> case dir of
      Horizontal -> "white-space: nowrap;"
      Vertical -> ""

    item k styl props children = [ Tuple k (HH.div ([itemStyle styl] <> props) children) ]
    adding i = join $ fromFoldable $ addBtn (Add i unit) <#> \b -> item ("add" <> show i) "" [] [ b ]
    isDragging dragging key = case dragging of
      Just { key: k } | k == key -> true
      _ -> false
    dragStyle :: forall r i. Maybe DragState -> Key -> String
    dragStyle dragging key = case dragging of
      Just { key: k, displacement, offset } | k == key ->
        let
          axis = case dir of
            Horizontal -> "X"
            Vertical -> "Y"
        in "transform: translate" <> axis <> "(" <> show (displacement - offset) <> "px)"
      _ -> ""

    render :: State e -> H.ParentHTML (Query o e) g p m
    render { values, dragging } = HK.div [topStyle] $ values #
      let top = length values - 1
          isDragged = isDragging dragging
      in surroundMapWithIndices adding \i (Tuple k v) ->
        let dragged = isDragged k in
        item k (dragStyle dragging k) [ HP.ref (label k) ]
          $ pure $ render1
            { prev: guard (i > 0) $> Swap i (i-1) unit
            , next: guard (i < top) $> Swap i (i+1) unit
            , remove: Remove k unit
            , set: \e -> Update k (const e) unit
            , modify: Update k <@> unit
            , output: Output <@> unit
            } (HE.onMouseDown (HE.input (Dragging k)))
            { key: k, index: i, value: v, dragged }

    mid = case dir of
      Vertical -> _.top `lift2 (+)` _.bottom >>> (_ / 2.0)
      Horizontal -> _.left `lift2 (+)` _.right >>> (_ / 2.0)

    -- getPos :: Key -> MaybeT (H.ComponentDSL (State e) (Query o e) (Output o e) m) Number
    getPos k = do
      e <- MaybeT $ H.getRef (label k)
      mid <$> H.liftEff (getBoundingClientRect (unsafeCoerce e))

    -- getPoses :: H.ComponentDSL (State e) (Query o e) (Output o e) m (Array (Maybe Number))
    getPoses =
      use _values >>=
        traverse \(Tuple k _) ->
          runMaybeT $ getPos k

    fresh' = fresh (prop (SProxy :: SProxy "supply"))

    notify = use _values >>= H.raise <<< Left <<< NewState <<< map extract

    eval :: Query o e ~> H.ParentDSL (State e) (Query o e) g p (Output o e) m
    eval (Output o next) = next <$ do
      H.raise $ Right o
    eval (Reset values next) = next <$ do
      H.liftEff $ log "Reset"
      H.put (initialState values)
    eval (Update k f next) = next <$ do
      _values %= map (extend \(Tuple k' v) -> if k == k' then f v else v)
      notify
    eval (Add i next) = next <$ do
      k <- append "item" <<< show <$> fresh'
      _values %= (fromMaybe <*> insertAt i (Tuple k default))
      notify
    eval (Remove k next) = next <$ do
      _values %= filter (fst >>> notEq k)
      notify
    eval (Swap i j next) = next <$ runMaybeT do
      values <- use _values
      a <- MaybeT $ pure (values !! i)
      b <- MaybeT $ pure (values !! j)
      v' <- MaybeT $ pure (updateAt i b values)
      v'' <- MaybeT $ pure (updateAt j a v')
      _values .= v''
      H.lift $ notify
    eval (DragTo i' next) = next <$ runMaybeT do
      dragging <- MaybeT $ use _dragging
      let k = dragging.key
      oldPos <- getPos k
      values <- use _values
      values' <- MaybeT $ pure do
        i <- findIndex (fst >>> eq k) values
        v <- values !! i
        deleteAt i values >>= insertAt i' v
      H.modify _ { values = values' }
      newPos <- getPos k
      H.liftEff $ log $ "dragTo offset: " <> show (newPos - oldPos)
      _offset += (newPos - oldPos)
      H.lift $ H.raise $ Left $ Preview (extract <$> values')
    eval (Dragging k e next) = next <$ runMaybeT do
      H.lift $ H.subscribe $ Drag.dragEventSource e \e' -> Just $ Move e' H.Listening
      _dragging ?= { key: k, displacement: 0.0, offset: 0.0 }
      H.lift $ H.raise $ Left $ DragStart
    eval (Move (Drag.Move e d) next) = next <$ do
      let
        mouseMovement = case dir of
          Horizontal -> d.offsetX
          Vertical -> d.offsetY
      H.liftEff $ logShow mouseMovement
      _dragging %= map _ { displacement = mouseMovement }
      poses <- getPoses
      runMaybeT do
        k <- MaybeT $ H.gets $ preview _dragKey
        values <- use _values
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
      H.liftEff $ log "End"
      _dragging .= Nothing
      H.raise $ Left $ DragEnd
      notify

data DemoQuery a
  = Receive (Message String) a

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
    btn :: forall q. Maybe (q Unit) -> String -> SimpleHTML q m
    btn q t = HH.button [ HE.onClick (pure q), HP.disabled (isNothing q) ] [ HH.text t ]
    add :: forall q. String -> q Unit -> Maybe (SimpleHTML q m)
    add t q = Just $ btn (Just q) t
    com1 = zuru Vertical mempty (add "Add") \{ next, prev, remove, set } -> \handle ->
      \{ key: k, index: i, value: v } -> HH.div_
        [ btn prev "▲"
        , HH.button [ handle, HP.attr (H.AttrName "style") "pointer: move" ] [ HH.text "≡" ]
        , btn next "▼"
        , HH.text (" " <> show (i+1) <> ". ")
        , HH.input
          [ HP.value v, HE.onValueInput (Just <<< set) ]
        , btn (Just remove) "-"
        ]
    com2 = zuru Horizontal mempty (add "+") \{ next, prev, remove, set } -> \handle ->
      \{ key: k, index: i, value: v } -> HH.div_
        [ HH.button [ handle, HP.attr (H.AttrName "style") "pointer: move" ] [ HH.text "≡" ]
        , HH.input
          [ HP.value v, HE.onValueInput (Just <<< set) ]
        , btn (Just remove) "-"
        ]

    update = H.put >>> (_ *> inform)
    inform = do
      r <- H.get
      H.liftEff $ logShow r

    lifting (Left m) = Just (Receive m unit)
    lifting (Right m) = Nothing

    render :: Array String -> H.ParentHTML DemoQuery (Query Void String) Int m
    render s = HH.div_ $
      [ HH.slot 1 com1 s lifting
      , HH.slot 2 com2 s lifting
      ]

    eval :: DemoQuery ~> H.ParentDSL (Array String) DemoQuery (Query Void String) Int v m
    eval (Receive (NewState v) a) = a <$ do
      H.liftEff $ log "Update"
      update v
    eval (Receive _ a) = pure a

type StateEl2D = Tuple String (Array String)
type State2D = Array StateEl2D

demo2 :: forall u v m eff.
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  H.Component HH.HTML (Tuple (Message StateEl2D)) u v m
demo2 =
  H.lifecycleParentComponent
    { initialState: const
      [ Tuple "This" ["a"]
      , Tuple "That" ["b"]
      , Tuple "These" ["a", "b"]
      ]
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    icon :: forall r i. HH.IProp ( "class" :: String | r ) i
    icon = HP.classes $ map H.ClassName $
      [ "material-icons" ]
    but :: forall r i. String -> Boolean -> HH.IProp ( "class" :: String | r ) i
    but c dis = HP.classes $ map H.ClassName $
      [ "btn", c ] <> (guard dis $> "disabled")
    handle_ :: forall r i. Boolean -> HH.IProp ( "class" :: String | r ) i
    handle_ b = HP.classes $ map H.ClassName $ [ "handle", size b ]

    size = if _ then "big" else "small"
    whenDragged = if _ then " dragged" else ""

    btn :: forall q f p. String -> Maybe (q Unit) -> String -> H.ParentHTML q f p m
    btn c q t = (compose (HH.a [but c (isNothing q)] <<< pure) <<< HH.i)
      [ icon, HE.onClick (pure q) ] [ HH.text t ]
    add :: forall q f p. Boolean -> String -> q Unit -> Maybe (H.ParentHTML q f p m)
    add b t q = Just $ btn ("add " <> size b) (Just q) t
    inner = zuru Vertical mempty (add false "+")
      \{ next, prev, remove, set } -> \handle ->
        \{ key: k, index: i, value: v, dragged } -> HH.div
          [ HP.class_ (H.ClassName $ "type" <> whenDragged dragged) ]
          [ btn "swap small" prev "keyboard_arrow_up"
          , HH.a [ handle, handle_ false ] [ HH.i [ icon ] [ HH.text "menu" ] ]
          , btn "swap small" next "keyboard_arrow_down"
          , HH.input
            [ HP.class_ $ H.ClassName "type"
            , HP.placeholder "Type of argument"
            , HP.value v
            , HE.onValueInput \v -> Just (set v)
            ]
          , btn "remove small" (Just remove) "remove"
          ]
    outer = zuruzuru Horizontal mempty (add true "create_new_folder")
      \{ next, prev, remove, modify } -> \handle ->
        \{ key: k, index: i, value: Tuple v cs, dragged } -> HH.div
          [ HP.class_ (H.ClassName $ "card constructor" <> whenDragged dragged) ]
          [ btn "swap big" prev "arrow_back"
          , HH.a [ handle, handle_ true ] [ HH.i [ icon ] [ HH.text "menu" ] ]
          , btn "swap big" next "arrow_forward"
          , HH.br_
          , HH.input
            [ HP.class_ $ H.ClassName "constructor"
            , HP.placeholder "Constructor name"
            , HP.value v
            , HE.onValueInput \v -> Just (modify (setl v))
            ]
          , HH.br_
          , HH.slot k inner cs (map modify <<< liftThru)
          , HH.br_
          , btn "remove big" (Just remove) "delete"
          ]

    update = H.put >>> (_ *> inform)
    inform = do
      r <- H.get
      H.liftEff $ logShow r

    setl a (Tuple _ b) = Tuple a b
    setr b (Tuple a _) = Tuple a b

    liftThru (Left (NewState cs)) = Just $ setr cs
    liftThru _ = Nothing

    lifting (Left m) = Just (Tuple m unit)
    lifting (Right _) = Nothing

    render :: State2D -> H.ParentHTML (Tuple (Message StateEl2D)) (Query Void StateEl2D) Unit m
    render s = HH.div [HP.class_ (H.ClassName "component") ] $
      [ HH.slot unit outer s lifting
      ]

    eval :: Tuple (Message StateEl2D) ~> H.ParentDSL State2D (Tuple (Message StateEl2D)) (Query Void StateEl2D) Unit v m
    eval (Tuple (NewState v) a) = a <$ do
      H.liftEff $ log "Update"
      update v
    eval (Tuple _ a) = pure a

-- | Demo app.
main :: forall e. Eff ( avar :: AVAR, ref :: REF, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE | e ) Unit
main = runHalogenAff $ unsafePartial do
  awaitLoad
  Just e <- selectElement (QuerySelector "#app")
  runUI demo2 unit e
