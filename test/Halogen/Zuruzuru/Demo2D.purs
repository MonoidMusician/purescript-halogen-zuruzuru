module Halogen.Zuruzuru.Demo2D where

import Prelude
import Data.Array (reverse, dropWhile)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow, log)
import Control.MonadPlus (guard)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Aff (awaitLoad, runHalogenAff, selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Zuruzuru (Direction(..), Key, Message(..), MuteInput, MuteSimpleInput, MuteSimpleSlot, MuteSlot, RenderAdder, _zuruzuru, justAfter, zuru, zuruzuru)
import Web.DOM.ParentNode (QuerySelector(..))
import Partial.Unsafe (unsafePartial)

type StateEl2D = Tuple String (Array String)
type State2D = Array StateEl2D

type ZZSlot3 m = ( zuruzuru2 :: MuteSimpleSlot m String Key )
type ZZSlot2 m = ( zuruzuru :: MuteSlot (ZZSlot3 m) m StateEl2D Unit )

demo2D :: forall u v m.
  MonadAff m =>
  H.Component HH.HTML (Tuple (Message StateEl2D)) u v m
demo2D =
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
  runUI demo2D unit e
