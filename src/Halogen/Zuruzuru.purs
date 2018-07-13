module Halogen.Zuruzuru
  ( zuruzuru
  , zuru
  , Keyed
  , Key
  , Direction(..)
  , Input
  , SimpleInput
  , MuteInput
  , MuteSimpleInput
  , Helpers
  , Handle
  , ItemInfo
  , RenderingInfoR
  , RenderingInfo
  , SimpleRenderingInfo
  , MuteRenderingInfo
  , MuteSimpleRenderingInfo
  , RenderAdderWhere
  , RenderAdder
  , RenderAdderIn
  , Message(..)
  , Output
  , MuteOutput
  , Query(..)
  , SimpleQuery
  , MuteQuery
  , MuteSimpleQuery
  -- , QueryChildF
  , State
  , StateR
  , RealState
  , RealSimpleState
  , MuteRealState
  , MuteRealSimpleState
  , DragState
  , NoSubOutput
  , NoSlots
  , everywhere
  , justAfter
  , justBefore
  , inside
  , main
  ) where


import Prelude

import Control.Comonad (extract)
import Control.Extend (extend)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Console (log, logShow)
import Control.Monad.Free (liftF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.MonadZero (guard)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Array (deleteAt, dropWhile, filter, findIndex, findLastIndex, foldMap, fromFoldable, insertAt, length, reverse, updateAt, (!!))
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Traversal', _Just, preview, use, (%=), (+=), (.=), (?=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
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
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.HalogenM as HM
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

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
type Input (ps :: # Type) m o e = RenderingInfo ps m o e ( values :: Array e )
-- | Simple input, where there are no child slots. (Helps with ambiguous type
-- | variables).
type SimpleInput m o e = Input NoSlots m o e
-- | Mute input, where there is no output.
type MuteInput ps m e = Input ps m NoSubOutput e
-- | Mute simple input, yeah.
type MuteSimpleInput m e = SimpleInput m NoSubOutput e

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

-- | Information about an item stored in state.
-- |
-- | - `key`: The unique key given to this item.
-- | - `index`: The current index of this item.
-- | - `value`: The current value of this item.
-- | - `dragged`: Whether this item is being dragged.
type ItemInfo e =
  { key :: String
  , index :: Int
  , value :: e
  , dragged :: Boolean
  }

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
type RenderAdder ps m = (forall q. RenderAdderIn q ps m)
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
data Query ps m o e a
  -- | Reset all the items in this component
  = Reset (Array e) a
  -- | Receive a new input
  | NewInput (Input ps m o e) a
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

type SimpleQuery = Query NoSlots
type MuteQuery ps m e = Query ps m NoSubOutput e
type MuteSimpleQuery m e = SimpleQuery m NoSubOutput e

-- | Used to fill for a child slot in simple components.
type NoSlots = ()

-- | Types for slots in the rows, following same convention
type Slot ps m o e = H.Slot (Query ps m o e) (Output o e)
type SimpleSlot m o e = Slot NoSlots m o e
type MuteSlot ps m e = Slot ps m NoSubOutput e
type MuteSimpleSlot m e = MuteSlot NoSlots m e

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
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input NewInput
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState = \{ direction, default, renderers, values } ->
      { direction, default, renderers
      , values: addKeys values
      , supply: length values
      , dragging: Nothing
      }

    label i = H.RefLabel ("zuruzuru-component" <> i) :: H.RefLabel

    itemStyle :: forall r i. String -> HP.IProp ( style :: String | r ) i
    itemStyle = HP.attr (H.AttrName "style")

    itemClass =  topClass

    dirClass :: Direction -> String
    dirClass = case _ of
      Horizontal -> "horizontal"
      Vertical -> "vertical"

    topClass :: forall r i. Direction -> HH.IProp ( "class" :: String | r ) i
    topClass = HP.classes <<< map H.ClassName <<< pure <<< dirClass

    dragStyleD :: Direction -> Maybe DragState -> Key -> String
    dragStyleD direction dragging key = case dragging of
      Just { key: k, displacement, offset } | k == key ->
        let
          axis = case direction of
            Horizontal -> "X"
            Vertical -> "Y"
        in "transform: translate" <> axis <> "(" <> show (displacement - offset) <> "px)"
      _ -> ""

    item dir k styl props children =
      [ Tuple k (HH.div ([itemClass dir, itemStyle styl] <> props) children) ]

    isDragging dragging key = case dragging of
      Just { key: k } | k == key -> true
      _ -> false

    render :: RealState ps m o e -> H.ComponentHTML (Query ps m o e) ps m
    render = \{ direction, default, renderers, values, dragging } ->
      HK.div [topClass direction] $ values #
        let
          top = length values - 1
          isDragged = isDragging dragging
          dragStyle = dragStyleD direction

          adding1 addBtn i = join $ fromFoldable $ addBtn (Add i unit) <#>
            \b -> item direction ("add" <> show i) "" [] [ b ]
          adding = mapRenderAdderWhere adding1 (chooseQuery renderers.adder)

        in surroundMapWithIndicesWhere adding \i (Tuple k v) ->
          let dragged = isDragged k in
          item direction k (dragStyle dragging k) [ HP.ref (label k) ]
            $ pure $ renderers.item
              { prev: guard (i > 0) $> Swap i (i-1) unit
              , next: guard (i < top) $> Swap i (i+1) unit
              , remove: Remove k unit
              , set: \e -> Update k (const e) unit
              , modify: Update k <@> unit
              , output: Output <@> unit
              } (HE.onMouseDown (HE.input (Dragging k)))
              { key: k, index: i, value: v, dragged }

    mid bb = do
      direction <- H.gets _.direction
      pure case direction of
        Vertical -> (bb.top + bb.bottom) / 2.0
        Horizontal -> (bb.left + bb.right) / 2.0

    -- getPos :: Key -> MaybeT (H.ComponentDSL (State e) (Query o e) (Output o e) m) Number
    getPos k = do
      e <- MaybeT $ H.getRef (label k)
      mid =<< H.liftEffect (getBoundingClientRect (unsafeCoerce e))

    -- getPoses :: H.ComponentDSL (State e) (Query o e) (Output o e) m (Array (Maybe Number))
    getPoses =
      use _values >>=
        traverse \(Tuple k _) ->
          runMaybeT $ getPos k

    notify = use _values >>= H.raise <<< Left <<< NewState <<< map extract

    eval :: Query ps m o e ~> H.HalogenM (RealState ps m o e) (Query ps m o e) ps (Output o e) m
    eval (Output o next) = next <$ do
      H.raise $ Right o
    eval (Reset values next) = next <$ do
      H.liftEffect $ log "Reset"
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
      H.liftEffect $ log $ "dragTo offset: " <> show (newPos - oldPos)
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
      H.liftEffect $ logShow mouseMovement
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
      H.liftEffect $ log "End"
      _dragging .= Nothing
      H.raise $ Left $ DragEnd
      notify
    eval (QueryChild cqbox) = HM.HalogenM $ liftF $ HM.ChildQuery cqbox

data DemoQuery a
  = Receive (Message String) a

type ZZSlot m = ( zuruzuru :: MuteSimpleSlot m String Int )

demo :: forall u v m.
  MonadAff m =>
  H.Component HH.HTML DemoQuery u v m
demo =
  H.component
    { initialState: const ["","",""]
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    btn :: forall q ps. Maybe (q Unit) -> String -> H.ComponentHTML q ps m
    btn q t = HH.button [ HE.onClick (pure q), HP.disabled (isNothing q) ] [ HH.text t ]

    add :: forall ps. String -> RenderAdder ps m
    add t q = Just $ btn (Just q) t

    com1 :: Array String -> MuteSimpleInput m String
    com1 =
      { values: _
      , direction: Vertical
      , default: pure mempty
      , renderers:
        { adder: justAfter (\q -> add "Add" q)
        , item: \{ next, prev, remove, set } -> \handle ->
          \{ key: k, index: i, value: v } -> HH.div_
            [ btn prev "▲"
            , HH.button [ handle, HP.attr (H.AttrName "style") "pointer: move" ] [ HH.text "≡" ]
            , btn next "▼"
            , HH.text (" " <> show (i+1) <> ". ")
            , HH.input
              [ HP.value v, HE.onValueInput (Just <<< set) ]
            , btn (Just remove) "-"
            ]
        }
      }

    com2 :: Array String -> MuteSimpleInput m String
    com2 =
      { values: _
      , direction: Horizontal
      , default: pure mempty
      , renderers:
        { adder: justAfter (add "+")
        , item: \{ next, prev, remove, set } -> \handle ->
          \{ key: k, index: i, value: v } -> HH.div_
            [ HH.button [ handle, HP.attr (H.AttrName "style") "pointer: move" ] [ HH.text "≡" ]
            , HH.input
              [ HP.value v, HE.onValueInput (Just <<< set) ]
            , btn (Just remove) "-"
            ]
        }
      }

    update = H.put >>> (_ *> inform)
    inform = do
      r <- H.get
      H.liftEffect $ logShow r

    lifting (Left m) = Just (Receive m unit)
    lifting (Right m) = Nothing

    render :: Array String -> H.ComponentHTML DemoQuery (ZZSlot m) m
    render s = HH.div_ $
      [ HH.slot _zuruzuru 1 zuru (com1 s) lifting
      , HH.slot _zuruzuru 2 zuru (com2 s) lifting
      ]

    eval :: DemoQuery ~> H.HalogenM (Array String) DemoQuery (ZZSlot m) v m
    eval (Receive (NewState v) a) = a <$ do
      H.liftEffect $ log "Update"
      update v
    eval (Receive _ a) = pure a

type StateEl2D = Tuple String (Array String)
type State2D = Array StateEl2D

type ZZSlot3 m = ( zuruzuru2 :: MuteSimpleSlot m String Key )
type ZZSlot2 m = ( zuruzuru :: MuteSlot (ZZSlot3 m) m StateEl2D Unit )

demo2 :: forall u v m.
  MonadAff m =>
  H.Component HH.HTML (Tuple (Message StateEl2D)) u v m
demo2 =
  H.component
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
    cl :: forall r i.  Array String -> HH.IProp ( "class" :: String | r ) i
    cl s = HP.classes $ map H.ClassName s

    but :: forall r i. Array String -> Boolean -> HH.IProp ( "class" :: String | r ) i
    but c dis = HP.classes $ map H.ClassName $
      ([ "btn"] <> c) <> (guard dis $> "disabled")

    handle_ :: forall r i. Boolean -> HH.IProp ( "class" :: String | r ) i
    handle_ b = HP.classes $ map H.ClassName $ [ "handle", size b ]

    size = if _ then "big" else "small"
    whenDragged = if _ then " dragged" else ""

    btn :: forall q ps. Array String -> Maybe (q Unit) -> String -> H.ComponentHTML q ps m
    btn c q t = HH.a
      [ but c (isNothing q), HE.onClick (pure q) ]
      [ icon t ]

    icon :: forall q ps. String -> H.ComponentHTML q ps m
    icon c = HH.i [ cl ["fa", "fa-"<> c ] ] [ ]

    add :: forall ps. Boolean -> RenderAdder ps m
    add b q = Just $ btn (["add", size b]) (Just q) "plus"

    inner :: Array String -> MuteSimpleInput m String
    inner =
      { values: _
      , direction: Vertical
      , default: pure mempty
      , renderers:
        { adder: justAfter (add false)
        , item: \{ next, prev, remove, set } -> \handle ->
          \{ key: k, index: i, value: v, dragged } ->
            HH.div
              [ HP.class_ (H.ClassName $ "type" <> whenDragged dragged) ]
              [ HH.div
                [HP.class_ $ H.ClassName "actions"]
                [ HH.a [ handle, handle_ false ] [ HH.text "■" ]
                , btn ["swap", "small"] prev "arrow-up"
                , btn ["swap", "small"] next "arrow-down"
                , btn ["remove", "small"] (Just remove) "remove"
                ]
              , HH.input
                [ HP.class_ $ H.ClassName "type"
                , HP.placeholder "Type of argument"
                , HP.value v
                , HE.onValueInput \v' -> Just (set v')
                ]
              ]
        }
      }

    outer :: State2D -> MuteInput (ZZSlot3 m) m StateEl2D
    outer =
      { values: _
      , direction: Vertical
      , default: pure mempty
      , renderers:
        { adder: justAfter (add true)
        , item:
          \{ next, prev, remove, modify } -> \handle ->
            \{ key: k, index: i, value: Tuple v cs, dragged } ->
              HH.div
                [ HP.class_ (H.ClassName $ "card constructor" <> whenDragged dragged) ]
                [ HH.div
                  [HP.class_ $ H.ClassName "actions"]
                  [ HH.a [ handle, handle_ true ] [ HH.text "■" ]
                  , btn ["swap","big"] prev "arrow-left"
                  , btn ["swap","big"] next "arrow-right"
                  , btn ["remove", "big"] (Just remove) "remove"
                  ]
                , HH.input
                  [ HP.class_ $ H.ClassName "constructor"
                  , HP.placeholder "Constructor name"
                  , HP.value v
                  , HE.onValueInput \v' -> Just (modify (setl v'))
                  ]
                , HH.slot (SProxy :: SProxy "zuruzuru2") k zuru (inner cs) (map modify <<< liftThru)
                ]
        }
      }

    update = H.put >>> (_ *> inform)
    inform = do
      r <- H.get
      H.liftEffect $ logShow r

    setl a (Tuple _ b) = Tuple a b
    setr b (Tuple a _) = Tuple a b

    addEmpty :: Array String -> Array String
    addEmpty = (_ <> [""]) <<< reverse <<< dropWhile (eq "") <<< reverse

    liftThru (Left (NewState cs)) = Just $ setr cs
    liftThru _ = Nothing

    lifting (Left m) = Just (Tuple m unit)
    lifting (Right _) = Nothing

    render :: State2D -> H.ComponentHTML (Tuple (Message StateEl2D)) (ZZSlot2 m) m
    render s = HH.div [HP.class_ (H.ClassName "component") ] $
      [ HH.slot _zuruzuru unit zuruzuru (outer s) lifting
      ]

    eval :: Tuple (Message StateEl2D) ~> H.HalogenM State2D (Tuple (Message StateEl2D)) (ZZSlot2 m) v m
    eval (Tuple (NewState v) a) = a <$ do
      H.liftEffect $ log "Update"
      update v
    eval (Tuple _ a) = pure a

-- | Demo app.
main :: Effect Unit
main = runHalogenAff $ unsafePartial do
  awaitLoad
  Just e <- selectElement (QuerySelector "#app")
  runUI demo2 unit e
