module Halogen.DnD where


import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM (DOM)
import Data.Array (deleteAt, insertAt, length, updateAt, (!!))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
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
  | Dragging Int a

type State =
  { values :: Array String
  , dragging :: Maybe
    { index :: Int
    , displacement :: Number
    }
  }

_values :: Lens' State (Array String)
_values = prop (SProxy :: SProxy "values")

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
  MonadEff ( dom :: DOM, console :: CONSOLE | eff ) m =>
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
    dragging i = HH.button
      [ HE.onMouseDown (HE.input_ (Dragging i)) ]
      [ HH.text "≡" ]

    render :: State -> H.ComponentHTML Query
    render { values } = HH.div_ $ values #
      surroundMapWithIndices adding \i v ->
        [ HH.div_
          [ but' (i > 0) "▲" (Swap i (i-1))
          , dragging i
          , but' (i < length values - 1) "▼" (Swap i (i+1))
          , HH.text (" " <> show (i+1) <> ". ")
          , HH.input
            [ HP.ref (label i)
            , HP.value v
            -- , HE.onInput (HE.input (Update <<< id))
            -- , HE.onClick (HE.input (Update <<< mouseEventToEvent))
            -- , HE.onKeyUp (HE.input (Update <<< keyboardEventToEvent))
            -- , HE.onFocus (HE.input (Update <<< focusEventToEvent))
            ]
          , but "-" (Remove i)
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query q m
    eval (Set values next) = next <$ do
      _values .= values
    eval (Add i next) = next <$ do
      _values %= (fromMaybe <*> insertAt i mempty)
    eval (Remove i next) = next <$ do
      _values %= (fromMaybe <*> deleteAt i)
    eval (Swap i j next) = next <$ runMaybeT do
      values <- H.lift $ H.gets (view _values)
      a <- MaybeT $ pure (values !! i)
      b <- MaybeT $ pure (values !! j)
      v' <- MaybeT $ pure (updateAt i b values)
      v'' <- MaybeT $ pure (updateAt j a v')
      _values .= v''
    eval (Dragging i next) = next <$ do
      H.liftEff $ logShow i

data DemoQuery a
  = Receive Void a
  | Reset (Array String) a

demo :: forall m eff.
  MonadEff ( dom :: DOM, console :: CONSOLE | eff ) m =>
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
