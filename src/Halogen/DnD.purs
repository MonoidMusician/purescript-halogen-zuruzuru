module Halogen.DnD where


import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Event.Event (target)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Event.Types (DragEvent)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (readHTMLElement)
import Data.Array (deleteAt, findIndex, findLastIndex, insertAt, length, updateAt, (!!))
import Data.Either (hush)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foreign (toForeign)
import Data.Lens (Lens', Traversal', _Just, preview, view, (%=), (+=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
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

data Query a
  = Set (Array String) a
  | Add Int a
  | Remove Int a
  | Swap Int Int a
  | Dragging Int MouseEvent a
  | Move Drag.DragEvent a
  | DragTo Int a
  | Update Int String a

type State =
  { values :: Array String
  , dragging :: Maybe DragState
  }

type DragState =
  { index :: Int
  , offset :: Number
  , displacement :: Number
  }

fresh :: forall s m. MonadState s m => Lens' s Int -> m Int
fresh lens = H.gets (view lens) <* H.modify (lens (_+1))

_values :: Lens' State (Array String)
_values = prop (SProxy :: SProxy "values")

_dragging :: Lens' State (Maybe DragState)
_dragging = prop (SProxy :: SProxy "dragging")

_dragIndex :: Traversal' State Int
_dragIndex = _dragging <<< _Just <<< prop (SProxy :: SProxy "index")

_offset :: Traversal' State Number
_offset = _dragging <<< _Just <<< prop (SProxy :: SProxy "offset")

initialState :: Array String -> State
initialState = { values: _, dragging: Nothing }

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
    label i = H.RefLabel ("textcursor-component" <> show i) :: H.RefLabel

    but t q = HH.button [ HE.onClick (HE.input_ q) ] [ HH.text t ]
    but' e t q = if e
      then but t q
      else HH.button [ HP.disabled true ] [ HH.text t ]

    add = HH.div_ [ but "+" (Add 0) ]
    adding i = [ HH.div_ [ but "+" (Add i) ] ]
    handle i = HH.button
      [ HE.onMouseDown (HE.input (Dragging i))
      , HP.attr (AttrName "style") "cursor: move"
      ]
      [ HH.text "≡" ]
    dragStyle :: forall r i. Maybe DragState -> Int -> HP.IProp r i
    dragStyle dragging idx = HP.attr (H.AttrName "style") case dragging of
      Just { index: i, displacement, offset } | i == idx ->
        "transform: translateY(" <> show (displacement - offset) <> "px)"
      _ -> ""

    render :: State -> H.ComponentHTML Query
    render { values, dragging } = HH.div_ $ values #
      surroundMapWithIndices adding \i v ->
        [ HH.div [ dragStyle dragging i ]
          [ but' (i > 0) "▲" (Swap i (i-1))
          , handle i
          , but' (i < length values - 1) "▼" (Swap i (i+1))
          , HH.text (" " <> show (i+1) <> ". ")
          , HH.input
            [ HP.ref (label i)
            , HP.value v
            , HE.onValueInput (HE.input (Update i <<< id))
            -- , HE.onClick (HE.input (Update <<< mouseEventToEvent))
            -- , HE.onKeyUp (HE.input (Update <<< keyboardEventToEvent))
            -- , HE.onFocus (HE.input (Update <<< focusEventToEvent))
            ]
          , but "-" (Remove i)
          ]
        ]

    mid = _.top `lift2 (+)` _.bottom >>> (_ / 2.0)

    getPos :: Int -> MaybeT (H.ComponentDSL State Query q m) Number
    getPos i = do
      e <- MaybeT $ H.getRef (label i)
      mid <$> H.liftEff (getBoundingClientRect (unsafeCoerce e))

    getPoses :: H.ComponentDSL State Query q m (Array (Maybe Number))
    getPoses =
      H.gets (view _values) >>=
        traverseWithIndex \i _ ->
          runMaybeT $ getPos i
          {-
          runMaybeT do
            e <- MaybeT $ H.getRef (label i)
            m <- mid <$> H.liftEff (getBoundingClientRect (unsafeCoerce e))
            v <- H.liftEff (value (unsafeCoerce e))
            pure {pos:m,val:v}
          -}

    eval :: Query ~> H.ComponentDSL State Query q m
    eval (Set values next) = next <$ do
      _values .= values
    eval (Update i v next) = next <$ do
      _values %= (fromMaybe <*> updateAt i v)
    eval (Add i next) = next <$ do
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
      _values %= (fromMaybe <*> insertAt i mempty)
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
    eval (Remove i next) = next <$ do
      -- getPoses >>= H.liftEff <<< log <<< unsafeCoerce
      _values %= (fromMaybe <*> deleteAt i)
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
      let i = dragging.index
      oldPos <- getPos i
      values <- H.gets (view _values)
      v <- MaybeT $ pure (values !! i)
      v' <- MaybeT $ pure (deleteAt i values)
      v'' <- MaybeT $ pure (insertAt i' v v')
      let dragging' = dragging { index = i' }
      H.put { values: v'', dragging: Just dragging' }
      newPos <- getPos i
      _offset += (oldPos - newPos)
    eval (Dragging i e next) = next <$ runMaybeT do
      -- H.liftEff $ logShow i
      H.lift $ H.subscribe $ Drag.dragEventSource e \e -> Just $ Move e Listening
      _dragging .= Just { index: i, displacement: 0.0, offset: 0.0 }
    eval (Move e next) = next <$ case e of
      Drag.Move e d -> do
        -- H.liftEff $ logShow d.offsetY
        _dragging %= map _ { displacement = d.offsetY }
        poses <- getPoses
        -- H.liftEff $ log $ unsafeCoerce poses
        void $ runMaybeT do
          i <- MaybeT $ H.gets $ preview _dragIndex
          p <- getPos i
          let
            -- these will either default to `i` (already) or
            -- be the index that this should go to
            least = fromMaybe i $ poses # findIndex (maybe false (_ >= p))
            most = fromMaybe i $ poses # findLastIndex (maybe false (_ <= p))
          -- H.liftEff $ logShow [least, i, most]
          guard (least < most)
          let
            i'
              | most > i = most
              | least < i = least
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
