module Halogen.Zuruzuru.Demo where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow, log)
import Halogen as H
import Halogen.Aff (awaitLoad, runHalogenAff, selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Zuruzuru (Direction(..), Message(..), MuteSimpleInput, MuteSimpleSlot, RenderAdder, _zuruzuru, justAfter, zuru)
import Web.DOM.ParentNode (QuerySelector(..))
import Partial.Unsafe (unsafePartial)

data DemoQuery a = Receive (Message String) a

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

-- | Demo app.
main :: Effect Unit
main = runHalogenAff $ unsafePartial do
  awaitLoad
  Just e <- selectElement (QuerySelector "#app")
  runUI demo unit e
