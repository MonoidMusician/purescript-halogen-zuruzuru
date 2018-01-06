module Halogen.DnD where


import Prelude

import Control.Apply (lift2)
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
import Data.Array (deleteAt, filter, findIndex, findLastIndex, insertAt, length, modifyAt, updateAt, (!!))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', Traversal', _Just, preview, view, (%=), (+=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Halogen (AttrName(..), SubscribeStatus(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)

type Keyed = Tuple String

data Query a
  = Set (Array String) a
  | Add Int a
  | Remove String a
  | Swap Int Int a
  | Dragging String MouseEvent a
  | Move Drag.DragEvent a
  | DragTo Int a
  | Update Int String a

type State =
  { values :: Array (Keyed String)
  , dragging :: Maybe DragState
  , supply :: Int
  }

type DragState =
  { key :: String
  , offset :: Number
  , displacement :: Number
  }

fresh :: forall s m. MonadState s m => Lens' s Int -> m Int
fresh lens = H.gets (view lens) <* H.modify (lens (_+1))

_values :: Lens' State (Array (Keyed String))
_values = prop (SProxy :: SProxy "values")

_dragging :: Lens' State (Maybe DragState)
_dragging = prop (SProxy :: SProxy "dragging")

_dragKey :: Traversal' State String
_dragKey = _dragging <<< _Just <<< prop (SProxy :: SProxy "key")

_offset :: Traversal' State Number
_offset = _dragging <<< _Just <<< prop (SProxy :: SProxy "offset")

initialState :: Array String -> State
initialState vs = { values: addKeys vs, supply: length vs, dragging: Nothing }

addKeys :: forall a. Array a -> Array (Keyed a)
addKeys = mapWithIndex (Tuple <<< show)

surroundMapWithIndices :: forall m a. Monoid m =>
  (Int -> m) ->
  (Int -> a -> m) ->
  Array a -> m
surroundMapWithIndices m f as = (_ <> m (length as)) $
  as # foldMapWithIndex \i a ->
    m i <> f i a

dnd :: forall m a q eff.
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  H.Component HH.HTML Query (Array String) q m
dnd =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: HE.input Set
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    label i = H.RefLabel ("textcursor-component" <> i) :: H.RefLabel

    but t q = HH.button [ HE.onClick (HE.input_ q) ] [ HH.text t ]
    but' e t q = if e
      then but t q
      else HH.button [ HP.disabled true ] [ HH.text t ]

    add = HH.div_ [ but "+" (Add 0) ]
    adding i = [ Tuple ("add" <> show i) $ HH.div_ [ but "+" (Add i) ] ]
    handle k = HH.button
      [ HE.onMouseDown (HE.input (Dragging k))
      , HP.attr (AttrName "style") "cursor: move"
      ]
      [ HH.text "≡" ]
    dragStyle :: forall r i. Maybe DragState -> String -> HP.IProp r i
    dragStyle dragging key = HP.attr (H.AttrName "style") case dragging of
      Just { key: k, displacement, offset } | k == key ->
        "transform: translateY(" <> show (displacement - offset) <> "px)"
      _ -> ""

    render :: State -> H.ComponentHTML Query
    render { values, dragging } = HK.div_ $ values #
      surroundMapWithIndices adding \i (Tuple k v) ->
        pure $ Tuple k $ HH.div [ dragStyle dragging k ]
          [ but' (i > 0) "▲" (Swap i (i-1))
          , handle k
          , but' (i < length values - 1) "▼" (Swap i (i+1))
          , HH.text (" " <> show (i+1) <> ". ")
          , HH.input
            [ HP.ref (label k)
            , HP.value v
            , HE.onValueInput (HE.input (Update i <<< id))
            -- , HE.onClick (HE.input (Update <<< mouseEventToEvent))
            -- , HE.onKeyUp (HE.input (Update <<< keyboardEventToEvent))
            -- , HE.onFocus (HE.input (Update <<< focusEventToEvent))
            ]
          , but "-" (Remove k)
          ]

    mid = _.top `lift2 (+)` _.bottom >>> (_ / 2.0)

    getPos :: String -> MaybeT (H.ComponentDSL State Query q m) Number
    getPos k = do
      e <- MaybeT $ H.getRef (label k)
      mid <$> H.liftEff (getBoundingClientRect (unsafeCoerce e))

    getPoses :: H.ComponentDSL State Query q m (Array (Maybe Number))
    getPoses =
      H.gets (view _values) >>=
        traverse \(Tuple k _) ->
          runMaybeT $ getPos k

    fresh' = fresh (prop (SProxy :: SProxy "supply"))

    eval :: Query ~> H.ComponentDSL State Query q m
    eval (Set values next) = next <$ do
      H.put (initialState values)
    eval (Update i v next) = next <$ do
      _values %= (fromMaybe <*> modifyAt i (_ $> v))
    eval (Add i next) = next <$ do
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
      k <- show <$> fresh'
      _values %= (fromMaybe <*> insertAt i (Tuple k mempty))
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
    eval (Remove k next) = next <$ do
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
      _values %= filter (fst >>> notEq k)
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
    eval (Swap i j next) = next <$ runMaybeT do
      values <- H.lift $ H.gets (view _values)
      a <- MaybeT $ pure (values !! i)
      b <- MaybeT $ pure (values !! j)
      v' <- MaybeT $ pure (updateAt i b values)
      v'' <- MaybeT $ pure (updateAt j a v')
      -- H.lift $ getPoses >>= H.liftEff <<< log <<< unsafeCoerce
      _values .= v''
      -- H.lift $ getPoses >>= H.liftEff <<< log <<< unsafeCoerce
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
      -- H.liftEff $ logShow i
      H.lift $ H.subscribe $ Drag.dragEventSource e \e -> Just $ Move e Listening
      H.gets (view _values) >>= H.liftEff <<< logShow
      _dragging .= Just { key: k, displacement: 0.0, offset: 0.0 }
    eval (Move e next) = next <$ case e of
      Drag.Move e d -> do
        -- H.liftEff $ logShow d.offsetY
        _dragging %= map _ { displacement = d.offsetY }
        poses <- getPoses
        -- H.liftEff $ log $ unsafeCoerce poses
        void $ runMaybeT do
          k <- MaybeT $ H.gets $ preview _dragKey
          values <- H.gets (view _values)
          i <- MaybeT $ pure $ findIndex (fst >>> eq k) values
          p <- getPos k
          let
            -- these will either default to `i` (already) or
            -- be the index that this should go to
            least = fromMaybe i $ poses # findIndex (maybe false (_ >= p))
            most = fromMaybe i $ poses # findLastIndex (maybe false (_ <= p))
          -- H.liftEff $ logShow [least, i, most]
          guard (least < most)
          let
            i'
              | most > i && least == i = most
              | least < i && most == i = least
              | otherwise = i
          guard (i /= i') -- redundant, but just to be safe
          -- H.liftEff $ logShow i'
          H.lift $ eval (DragTo i' unit)
      Drag.Done e -> do
        -- H.liftEff $ log "Done"
        _dragging .= Nothing

data DemoQuery a
  = Receive Void a
  | Reset (Array String) a

demo :: forall m eff.
  MonadAff ( dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF | eff ) m =>
  H.Component HH.HTML DemoQuery Unit Void m
demo =
  H.lifecycleParentComponent
    { initialState: const ["1","2","3"]
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    com = dnd

    update = H.put >>> (_ *> inform)
    inform = do
      r <- H.get
      H.liftEff $ log $ unsafeCoerce r

    render :: Array String -> H.ParentHTML DemoQuery Query Unit m
    render s = HH.div_ [HH.slot unit com s (HE.input Receive)]

    eval :: DemoQuery ~> H.ParentDSL (Array String) DemoQuery Query Unit Void m
    eval (Reset v a) = a <$ do
      update v
    eval (Receive v a) = pure a

main :: forall e. Eff ( avar :: AVAR, ref :: REF, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE | e ) Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit
