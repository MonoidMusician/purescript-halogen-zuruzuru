module Halogen.Zuruzuru
  ( zuruzuru
  , zuru
  , zuruzuru'
  , Keyed
  , Key
  , Direction(..)
  , InputN
  , Input
  , SimpleInput
  , MuteInput
  , MuteSimpleInput
  , Input'
  , SimpleInput'
  , MuteInput'
  , MuteSimpleInput'
  , Helpers
  , Handle
  , Handle'
  , ItemInfoR
  , ItemInfo
  , ItemInfo'
  , RenderingInfoR
  , RenderingInfo
  , SimpleRenderingInfo
  , MuteRenderingInfo
  , MuteSimpleRenderingInfo
  , Renderer(..)
  , RenderingInfoR'
  , RenderingInfo'
  , SimpleRenderingInfo'
  , MuteRenderingInfo'
  , MuteSimpleRenderingInfo'
  , RenderAdderWhere
  , RenderAdder
  , RenderAdderIn
  , Message(..)
  , Output
  , MuteOutput
  , QueryI(..)
  , Query
  , SimpleQuery
  , MuteQuery
  , MuteSimpleQuery
  , Query'
  , SimpleQuery'
  , MuteQuery'
  , MuteSimpleQuery'
  , Slot
  , SimpleSlot
  , MuteSlot
  , MuteSimpleSlot
  , Slot'
  , SimpleSlot'
  , MuteSlot'
  , MuteSimpleSlot'
  , State
  , StateR
  , RealState
  , RealSimpleState
  , MuteRealState
  , MuteRealSimpleState
  , DragState
  , NoSubOutput
  , NoSlots
  , queryInside
  , queryAllInside
  , everywhere
  , justAfter
  , justBefore
  , inside
  , _zuruzuru
  ) where


import Prelude

import Control.Comonad (extract)
import Control.Extend (extend)
import Control.Monad.Free (liftF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Control.MonadZero (class Plus, empty, guard)
import Data.Array (deleteAt, filter, findIndex, findLastIndex, foldMap, fromFoldable, insertAt, length, updateAt, (!!))
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Traversal', _Just, preview, use, (%=), (+=), (.=), (?=))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Profunctor (dimap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, logShow)
import Halogen as H
import Halogen.Component.Profunctor (ProComponent(..))
import Halogen.Component.Utils.Drag as Drag
import Halogen.Data.Slot as Slot
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.HalogenM as HM
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.UIEvent.MouseEvent (MouseEvent)

-- | An element with a key (string), pretty simple.
type Keyed = Tuple Key

-- | ItemInfos are kept track of with a string key, for Halogen's keyed elements.
type Key = String

-- | The direction to render the list.
data Direction = Vertical | Horizontal

-- | The input to the component. In general, parameters are named like this:
-- |
-- | - `ps`: The type of child slots, or `NoSlots = ()` if none.
-- | - `m`: The monad this component runs in.
-- | - `o`: Any other output passed through the component, from the rendered
-- |    `HTML` to the output type of the zuruzuru component.
-- | - `e`: The type of items being rendered and edited in this component.
type Input ps m o e = RenderingInfo ps m o e ( values :: Array e )
newtype InputN ps m o e = InputN (Input ps m o e)
-- | Simple input, where there are no child slots. (Helps with ambiguous type
-- | variables).
type SimpleInput m o e = Input NoSlots m o e
-- | Mute input, where there is no output.
type MuteInput ps m e = Input ps m NoSubOutput e
-- | Mute simple input, yeah.
type MuteSimpleInput m e = SimpleInput m NoSubOutput e

type Input' (ps :: # Type) m o e = RenderingInfo' ps m o e ( values :: Array e )
type SimpleInput' m o e = Input' NoSlots m o e
type MuteInput' ps m e = Input' ps m NoSubOutput e
type MuteSimpleInput' m e = SimpleInput' m NoSubOutput e

-- | The (opaque) queries to perform certain actions on a particular item.
-- |
-- | - `remove`: Remove this item.
-- | - `modify`: Change this item's value based on its previous value.
-- | - `set`: Give this item a new value.
-- | - `prev`: Swap this item with the previous one, if it is not the first.
-- | - `next`: Swap this item with the next one, if not the last.
-- | - `output`: Lift an output query through the component.
type Helpers o q e =
  { prev :: Maybe (q Unit)
  , next :: Maybe (q Unit)
  , modify :: (e -> e) -> q Unit
  , remove :: q Unit
  , set :: e -> q Unit
  , output :: o -> q Unit
  }

-- | The property for a handler. Not in the `Helpers` record because records
-- | hate impredicativity.
type Handle q = forall r. HH.IProp ( onMouseDown :: MouseEvent | r ) (q Unit)

type Handle' q =
  { label :: H.RefLabel
  , onMouseDown :: MouseEvent -> Maybe (q Unit)
  }

-- | Information about an item stored in state.
-- |
-- | - `key`: The unique key given to this item.
-- | - `index`: The current index of this item.
-- | - `value`: The current value of this item.
-- | - `dragged`: Whether this item is being dragged.
type ItemInfoR e r =
  ( key :: String
  , index :: Int
  , value :: e
  , dragged :: Boolean
  | r
  )
type ItemInfo e = Record (ItemInfoR e ())

type ItemInfo' e = Record (ItemInfoR e ( offset :: Number ))

-- | All of this data is provided to the component on every updated and stored
-- | in state, so it can be updated while the component is running.
-- |
-- | - `direction`: Whether the component renders vertically or horizontally.
-- | - `default`: The default value for a new item. May be monadic.
-- | - `renderers.adder`: Where and how to render buttons to add new items. See
-- |    below.
-- | - `renderers.item`: How to render an item.
type RenderingInfoR ps m o e r =
  ( direction :: Direction
  , default :: m e
  , renderers ::
    { adder :: RenderAdderWhere (RenderAdder ps m)
    , item :: forall q. Helpers o q e -> Handle q -> ItemInfo e -> H.ComponentHTML q ps m
    }
  | r
  )
-- | The same thing but as a record.
type RenderingInfo ps m o e r = Record (RenderingInfoR ps m o e r)
-- | The record when there are no child components.
type SimpleRenderingInfo m o e r = RenderingInfo NoSlots m o e r
-- | The record when output is nil.
type MuteRenderingInfo ps m e r = RenderingInfo ps m NoSubOutput e r
-- | The combination of the above.
type MuteSimpleRenderingInfo m e r = SimpleRenderingInfo m NoSubOutput e r

newtype Renderer ps m o e = Renderer
  forall q.
    Array
      { helpers :: Helpers o q e
      , handle :: Handle' q
      , info :: ItemInfo' e
      } ->
    { add :: Int -> q Unit
    , output :: o -> q Unit
    } ->
    H.ComponentHTML q ps m
type RenderingInfoR' ps m o e r =
  ( direction :: Direction
  , default :: m e
  , renderer :: Renderer ps m o e
  | r
  )
type RenderingInfo' ps m o e r = Record (RenderingInfoR' ps m o e r)
type SimpleRenderingInfo' m o e r = RenderingInfo' NoSlots m o e r
type MuteRenderingInfo' ps m e r = RenderingInfo' ps m NoSubOutput e r
type MuteSimpleRenderingInfo' m e r = SimpleRenderingInfo' m NoSubOutput e r

-- | Where and how to render an element to add an item. Set to `Nothing` to
-- | avoid rendering something, otherwise provide a renderer with `mkJust`.
-- | Actually best to use the prebuilt constructors that handle common cases.
-- |
-- | `before` means that (if there is at least one item) an adder will be
-- | rendered before the start of the list; `between` means an adder will be
-- | rendered between each item in the list; and `after` renders after the
-- | whole list (which may be possibly empty).
type RenderAdderWhere a =
  { before :: Maybe a
  , between :: Maybe a
  , after :: Maybe a
  }

-- | The type of a rendering function, which does not have access to the query
-- | type. Impredicative, be warned!
type RenderAdder ps m = forall q. RenderAdderIn q ps m
-- | The type of a rendering function when supplied with a specific query type.
type RenderAdderIn q ps m = q Unit -> Maybe (H.ComponentHTML q ps m)

-- | The output message from the component.
data Message e
  = NewState (Array e)
  | Preview (Array e)
  | DragStart
  | DragEnd

-- | Either a message from the component or the out passed through the component.
type Output o e = Either (Message e) o
-- | Combine `Output` with `NoSubOutput`
type MuteOutput e = Output NoSubOutput e
-- | Another `Void` synonym, for when the rendered HTML has no output of its own.
type NoSubOutput = Void

-- | Query for the component.
data QueryI i ps o e a
  -- | Reset all the items in this component
  = Reset (Array e) a
  -- | Receive a new input
  | NewInput i a
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
  -- | Transparently query a subcomponent
  | QueryChild (CQ.ChildQueryBox ps a)

derive instance functorQueryI :: Functor (QueryI ps m o e)

type Query ps m o e = QueryI (InputN ps m o e) ps o e
type SimpleQuery m o e = Query NoSlots m o e
type MuteQuery ps m e = Query ps m NoSubOutput e
type MuteSimpleQuery m e = SimpleQuery m NoSubOutput e

type Query' ps m o e = QueryI (Input' ps m o e) ps o e
type SimpleQuery' m o e = Query' NoSlots m o e
type MuteQuery' ps m e = Query' ps m NoSubOutput e
type MuteSimpleQuery' m e = SimpleQuery' m NoSubOutput e

-- | Used to fill for a child slot in simple components.
type NoSlots = ()

-- | Types for slots in the rows, following same convention
type Slot ps m o e = H.Slot (Query ps m o e) (Output o e)
type SimpleSlot m o e = Slot NoSlots m o e
type MuteSlot ps m e = Slot ps m NoSubOutput e
type MuteSimpleSlot m e = MuteSlot NoSlots m e

type Slot' ps m o e = H.Slot (Query' ps m o e) (Output o e)
type SimpleSlot' m o e = Slot' NoSlots m o e
type MuteSlot' ps m e = Slot' ps m NoSubOutput e
type MuteSimpleSlot' m e = MuteSlot' NoSlots m e

-- | Extensible *row* for the state of the component.
type StateR e r =
  ( values :: Array (Keyed e)
  , dragging :: Maybe DragState
  , supply :: Int
  | r
  )
-- | Extensible *record* for the state of the component.
type State e r = Record (StateR e r)
-- | The full state, taking into account the rendering info too.
type RealState ps m o e = State e (RenderingInfoR ps m o e ())
-- | The full state for a non-parent component.
type RealSimpleState m o e = RealState NoSlots m o e
-- | The full state when there is no output.
type MuteRealState ps m e = RealState ps m NoSubOutput e
-- | The full state for a non-parent component with output just from this component.
type MuteRealSimpleState m e = RealSimpleState m NoSubOutput e

type RealState' ps m o e = State e (RenderingInfoR' ps m o e ())
type RealSimpleState' m o e = RealState' NoSlots m o e
type MuteRealState' ps m e = RealState' ps m NoSubOutput e
type MuteRealSimpleState' m e = RealSimpleState' m NoSubOutput e

-- | State maintained while dragging. Together the numbers are used to
-- | translate the corresponding item (using a CSS transform).
-- |
-- | - `key`: The key of the item being dragged.
-- | - `offset`: The offset from its original position, whence it was dragged.
-- | - `displacement`: The displacement of the mouse from the start of the drag.
type DragState =
  { key :: String
  , offset :: Number
  , displacement :: Number
  }

-- | Helper, this is a proof that (despite impredicativity) we can select a
-- | particular query type for the abstracted one that we required the caller
-- | to give.
chooseQuery :: forall ps m q. RenderAdderWhere (RenderAdder ps m) -> RenderAdderWhere (RenderAdderIn q ps m)
chooseQuery { before, between, after } =
  { before: before <#> \q -> q
  , between: between <#> \q -> q
  , after: after <#> \q -> q
  }

-- | Make a `Just` renderer, helps with inference.
mkJust :: forall ps m. RenderAdder ps m -> Maybe (RenderAdder ps m)
mkJust = Just :: RenderAdder ps m -> Maybe (RenderAdder ps m)

-- | Render an adder just after the list.
justAfter :: forall ps m. RenderAdder ps m -> RenderAdderWhere (RenderAdder ps m)
justAfter a = { before: Nothing, between: Nothing, after: mkJust a }

justAfter' :: forall a. a -> RenderAdderWhere a
justAfter' a = { before: Nothing, between: Nothing, after: Just a }

justBefore :: forall ps m. RenderAdder ps m -> RenderAdderWhere (RenderAdder ps m)
justBefore a = { before: Nothing, between: Nothing, after: mkJust a }

justBefore' :: forall a. a -> RenderAdderWhere a
justBefore' a = { before: Just a, between: Nothing, after: Nothing }

inside :: forall ps m. RenderAdder ps m -> RenderAdderWhere (RenderAdder ps m)
inside a = { before: Nothing, between: mkJust a, after: Nothing }

inside' :: forall a. a -> RenderAdderWhere a
inside' a = { before: Nothing, between: Just a, after: Nothing }

everywhere :: forall ps m. RenderAdder ps m -> RenderAdderWhere (RenderAdder ps m)
everywhere a = { before: mkJust a, between: mkJust a, after: mkJust a }

everywhere' :: forall a. a -> RenderAdderWhere a
everywhere' a = { before: Just a, between: Just a, after: Just a }

mapRenderAdderWhere :: forall a b. (a -> b) -> RenderAdderWhere a -> RenderAdderWhere b
mapRenderAdderWhere f { before, between, after } =
  { before: f <$> before
  , between: f <$> between
  , after: f <$> after
  }

_values :: forall e r. Lens' (State e r) (Array (Keyed e))
_values = prop (SProxy :: SProxy "values")

_dragging :: forall e r. Lens' (State e r) (Maybe DragState)
_dragging = prop (SProxy :: SProxy "dragging")

_dragKey :: forall e r. Traversal' (State e r) String
_dragKey = _dragging <<< _Just <<< prop (SProxy :: SProxy "key")

_offset :: forall e r. Traversal' (State e r) Number
_offset = _dragging <<< _Just <<< prop (SProxy :: SProxy "offset")

addKeys :: forall e. Array e -> Array (Keyed e)
addKeys = mapWithIndex (Tuple <<< append "item" <<< show)

surroundMapWithIndicesWhere :: forall m a. Monoid m =>
  RenderAdderWhere (Int -> m) ->
  (Int -> a -> m) ->
  Array a -> m
surroundMapWithIndicesWhere { before, after, between } f as =
  let
    notAfter =
      as # foldMapWithIndex \i a ->
        let addSpacer = if i == 0 then before else between
        in foldMap (_ $ i) addSpacer <> f i a
  in notAfter <> foldMap (_ $ length as) after

surroundMapWhere :: forall m a. Monoid m =>
  RenderAdderWhere (Int -> m) ->
  (a -> m) ->
  Array a -> m
surroundMapWhere { before, after, between } f as =
  let
    notAfter =
      as # foldMapWithIndex \i a ->
        let addSpacer = if i == 0 then before else between
        in foldMap (_ $ i) addSpacer <> f a
  in notAfter <> foldMap (_ $ length as) after

_zuruzuru = SProxy :: SProxy "zuruzuru"

-- | `zuruzuru` minus the higher-order parent component junk.
zuru :: forall m o e.
  MonadAff m =>
  H.Component HH.HTML (SimpleQuery m o e) (SimpleInput m o e) (Output o e) m
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
-- |     information about the item (including its position, see `ItemInfo`)
zuruzuru :: forall ps m o e.
  MonadAff m =>
  H.Component HH.HTML (Query ps m o e) (Input ps m o e) (Output o e) m
zuruzuru =
  ctransQuery (mapQueryI (\(InputN i) -> cmapI i)) $
    un ProComponent $ dimap cmapI identity $ ProComponent zuruzuru'
  where
    cmapI :: Input ps m o e -> Input' ps m o e
    cmapI { direction, default, values, renderers } =
      { direction, default, values
      , renderer: Renderer \datums q ->
          HK.div [topClass direction] $ datums #
            let
              item dir k styl props children =
                [ Tuple k (HH.div ([itemClass dir, itemStyle styl] <> props) children) ]

              top = length datums - 1
              dragStyle = dragStyleD direction

              adding1 addBtn i = join $ fromFoldable $ addBtn (q.add i) <#>
                \b -> item direction ("add" <> show i) "" [] [ b ]
              adding = mapRenderAdderWhere adding1 (chooseQuery renderers.adder)

            in surroundMapWhere adding \datum@{ helpers, info: info@{ index: i, key: k, value: v } } ->
              item direction k (dragStyle info.dragged info.offset)
                [ HP.ref datum.handle.label ]
                $ pure $ renderers.item helpers (HE.onMouseDown datum.handle.onMouseDown)
                  { key: k, index: i, value: v, dragged: info.dragged }
      }

    itemStyle :: forall r i. String -> HP.IProp ( style :: String | r ) i
    itemStyle = HP.attr (H.AttrName "style")

    itemClass =  topClass

    topClass :: forall r i. Direction -> HH.IProp ( "class" :: String | r ) i
    topClass = HP.classes <<< map H.ClassName <<< pure <<< dirClass

    dragStyleD :: Direction -> Boolean -> Number -> String
    dragStyleD direction dragged offset = case dragged of
      true ->
        let
          axis = case direction of
            Horizontal -> "X"
            Vertical -> "Y"
        in "transform: translate" <> axis <> "(" <> show offset <> "px)"
      _ -> ""

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
-- |     information about the item (including its position, see `ItemInfo`)
zuruzuru' :: forall ps m o e.
  MonadAff m =>
  H.Component HH.HTML (Query' ps m o e) (Input' ps m o e) (Output o e) m
zuruzuru' =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input NewInput
    , initializer: Nothing
    , finalizer: Nothing
    }

initialState :: forall ps m o e.Input' ps m o e -> RealState' ps m o e
initialState = \{ direction, default, renderer, values } ->
  { direction, default, renderer
  , values: addKeys values
  , supply: length values
  , dragging: Nothing
  }

label :: String -> H.RefLabel
label i = H.RefLabel ("zuruzuru-component" <> i)

dirClass :: Direction -> String
dirClass = case _ of
  Horizontal -> "horizontal"
  Vertical -> "vertical"

render :: forall ps m o e. RealState' ps m o e -> H.ComponentHTML (Query' ps m o e) ps m
render = \{ direction, default, renderer: Renderer renderer, values, dragging } ->
  renderer <@> { add: Add <@> unit, output: Output <@> unit } $ values #
    let
      top = length values - 1
      element_offset = case dragging of
        Just { displacement, offset } -> displacement - offset
        _ -> zero
      isDragged = isDragging dragging

    in mapWithIndex \i (Tuple k v) ->
      let dragged = isDragged k in
      { helpers:
        { prev: guard (i > 0) $> Swap i (i-1) unit
        , next: guard (i < top) $> Swap i (i+1) unit
        , remove: Remove k unit
        , set: \e -> Update k (const e) unit
        , modify: Update k <@> unit
        , output: Output <@> unit
        }
      , handle:
        { label: label k
        , onMouseDown: HE.input $ Dragging k
        }
      , info:
        { key: k, index: i, value: v, dragged
        , offset: if dragged then element_offset else zero
        }
      }
  where
    isDragging dragging key = case dragging of
      Just { key: k } | k == key -> true
      _ -> false

mid ::
  forall m r.
    Plus m =>
    MonadState { direction :: Direction | r } m =>
  { width :: Number
  , top :: Number
  , right :: Number
  , left :: Number
  , height :: Number
  , bottom :: Number
  } ->
  m Number
mid { bottom: 0.0, height: 0.0, left: 0.0, right: 0.0, top: 0.0, width: 0.0 } = empty
mid bb = H.gets $ _.direction >>> case _ of
    Vertical -> (bb.top + bb.bottom) / 2.0
    Horizontal -> (bb.left + bb.right) / 2.0

getPos :: forall ps m o e query output. MonadAff m =>
  Key -> MaybeT (H.HalogenM (RealState' ps m o e) query ps output m) Number
getPos k = do
  e <- MaybeT $ H.getRef (label k)
  mid =<< H.liftEffect (getBoundingClientRect (unsafeCoerce e))

-- getPoses :: H.ComponentDSL (State e) (Query o e) (Output o e) m (Array (Maybe Number))
getPoses :: forall ps m o e query output. MonadAff m =>
  H.HalogenM (RealState' ps m o e) query ps output m (Array (Maybe Number))
getPoses =
  use _values >>=
    traverse \(Tuple k _) ->
      runMaybeT $ getPos k


notify :: forall ps m o e. H.HalogenM (RealState' ps m o e) (Query' ps m o e) ps (Output o e) m Unit
notify = H.raise <<< Left <<< NewState <<< map extract =<< use _values

eval :: forall ps m o e. MonadAff m => Query' ps m o e ~> H.HalogenM (RealState' ps m o e) (Query' ps m o e) ps (Output o e) m
eval (Output o next) = next <$ do
  H.raise $ Right o
eval (Reset values next) = next <$ do
  log "Reset"
  H.modify_ _ { values = addKeys values, supply = length values }
eval (NewInput values next) = next <$ do
  H.put (initialState values)
eval (Update k f next) = next <$ do
  _values %= map (extend \(Tuple k' v) -> if k == k' then f v else v)
  notify
eval (Add i next) = next <$ do
  sup <- H.gets _.supply
  let k = show sup
  H.modify_ _ { supply = sup + 1 }
  v <- H.lift =<< H.gets _.default
  _values %= (fromMaybe <*> insertAt i (Tuple k v))
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
  H.modify_ _ { values = values' }
  newPos <- getPos k
  log $ "dragTo offset: " <> show (newPos - oldPos)
  _offset += (newPos - oldPos)
  H.lift $ H.raise $ Left $ Preview (extract <$> values')
eval (Dragging k e next) = next <$ runMaybeT do
  void $ H.lift $ H.subscribe $ Drag.dragEventSource e \e' -> Just $ Move e' unit
  _dragging ?= { key: k, displacement: 0.0, offset: 0.0 }
  H.lift $ H.raise $ Left $ DragStart
eval (Move (Drag.Move e d) next) = next <$ do
  direction <- H.gets _.direction
  let
    mouseMovement = case direction of
      Horizontal -> d.offsetX
      Vertical -> d.offsetY
  logShow mouseMovement
  _dragging %= map _ { displacement = mouseMovement }
  poses <- getPoses
  logShow poses
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
  log "End"
  _dragging .= Nothing
  H.raise $ Left $ DragEnd
  notify
eval (QueryChild cqbox) = HM.HalogenM $ liftF $ HM.ChildQuery cqbox

mapQueryI :: forall i i' ps o e.
  (i -> i') ->
  QueryI i ps o e ~>
  QueryI i' ps o e
mapQueryI f = case _ of
  Reset vs a -> Reset vs a
  NewInput i a -> NewInput (f i) a
  Add i a -> Add i a
  Update k u a -> Update k u a
  Remove k a -> Remove k a
  Swap i j a -> Swap i j a
  Dragging k e a -> Dragging k e a
  Move m a -> Move m a
  DragTo i' a -> DragTo i' a
  Output o a -> Output o a
  QueryChild cqbox -> QueryChild cqbox

ctransQuery :: forall h f f' i o m.
  (f' ~> f) ->
  H.Component h f i o m ->
  H.Component h f' i o m
ctransQuery t = H.unComponent \c -> H.mkComponent
  { initialState: c.initialState, render: c.render
  , eval: transHalogenQ t >>> c.eval
  }

transHalogenQ :: forall f f' act i a.
  (f a -> f' a) ->
  H.HalogenQ f act i a ->
  H.HalogenQ f' act i a
transHalogenQ t = case _ of
  H.Initialize a -> H.Initialize a
  H.Finalize a -> H.Finalize a
  H.Receive i a -> H.Receive i a
  H.Handle act a -> H.Handle act a
  H.Request fa -> H.Request (t fa)

queryInside
  :: forall sym f o' p px ps m o e a
   . Row.Cons sym (Slot.Slot f o' p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> f a
  -> Query ps m o e (Maybe a)
queryInside sym p q = QueryChild $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k -> traverse k <<< Slot.lookup sym p) q identity

queryAllInside
  :: forall sym f o' p px ps m o e a
   . Row.Cons sym (Slot.Slot f o' p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> f a
  -> Query ps m o e (Map p a)
queryAllInside sym q = QueryChild $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k -> traverse k <<< Slot.slots sym) q identity
