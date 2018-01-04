module Halogen.TextCursor
    ( textCursorComponent
    , Query(..)
    , TCInputType(..)
    , main
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import DOM (DOM)
import DOM.Event.Types (Event, focusEventToEvent, keyboardEventToEvent, mouseEventToEvent)
import DOM.Util.TextCursor (Direction(None), TextCursor(TextCursor), content)
import DOM.Util.TextCursor.Element (setTextCursor, textCursor, validate')
import DOM.Util.TextCursor.Element.Type (read', readEventTarget)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)

data Query a
  = Init a
  | FromEvent TextCursor a
  | Update Event a
  | FromOutside TextCursor a

data TCInputType
  = TCTextArea
  | TCInput
  | TCEmail
  | TCSearch
  | TCUrl

toInputType :: TCInputType -> Maybe InputType
toInputType = case _ of
  TCTextArea -> Nothing
  TCInput -> Just InputText
  TCEmail -> Just InputEmail
  TCSearch -> Just InputSearch
  TCUrl -> Just InputUrl

textCursorComponent :: forall m eff.
  MonadEff ( dom :: DOM | eff ) m =>
  TCInputType ->
  H.Component HH.HTML Query TextCursor TextCursor m
textCursorComponent typ =
  H.lifecycleComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input FromOutside
    , initializer: Just (Init unit)
    , finalizer: Nothing
    }
  where
    label = H.RefLabel "textcursor-component" :: H.RefLabel
    render :: TextCursor -> H.ComponentHTML Query
    render = case toInputType typ of
      Nothing -> \tc ->
        HH.textarea
          [ HP.ref label
          , HP.value (content tc)
          ]
      Just ty -> \tc ->
        HH.input
          [ HP.ref label
          , HP.type_ ty
          , HP.value (content tc)
          , HE.onInput (HE.input (Update <<< id))
          , HE.onClick (HE.input (Update <<< mouseEventToEvent))
          , HE.onKeyUp (HE.input (Update <<< keyboardEventToEvent))
          , HE.onFocus (HE.input (Update <<< focusEventToEvent))
          ]

    eval :: Query ~> H.ComponentDSL TextCursor Query TextCursor m
    eval (Init next) = do
      tc <- H.get
      eval (FromOutside tc next)
    eval (FromEvent tc next) = next <$ do
      cur <- H.get
      when (cur /= tc) do
        H.put tc <* H.raise tc
    eval (Update e next) = next <$ runMaybeT do
      elem <- MaybeT $ H.liftEff $ validate' (readEventTarget e)
      tc <- H.liftEff $ textCursor elem
      lift $ eval (FromEvent tc unit)
    eval (FromOutside tc next) = next <$ runMaybeT do
      e <- MaybeT $ H.getRef label
      elem <- MaybeT $ H.liftEff $ validate' (read' e)
      H.liftEff $ setTextCursor tc elem
      H.put tc

data DemoQuery a
  = Set TextCursor a
  | Receive TextCursor a

demo :: forall m eff.
  MonadEff ( dom :: DOM, console :: CONSOLE | eff ) m =>
  H.Component HH.HTML DemoQuery Unit Void m
demo =
  H.lifecycleParentComponent
    { initialState: const nov
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    nov = TextCursor
      { before: "before"
      , selected: "(selected)"
      , after: "after"
      , direction: None
      }
    com = textCursorComponent TCInput

    update = H.put >>> (_ *> inform)
    inform = do
      TextCursor r <- H.get
      H.liftEff $ log $ unsafeCoerce [r.before, r.selected, r.after]

    render :: TextCursor -> H.ParentHTML DemoQuery Query Unit m
    render tc = HH.slot unit com tc (HE.input Receive)

    eval :: DemoQuery ~> H.ParentDSL TextCursor DemoQuery Query Unit Void m
    eval (Set tc a) = a <$ do
      update tc
    eval (Receive tc a) = a <$ do
      update tc
      when (content tc == "reset") do
        eval (Set nov unit)

main :: forall e. Eff ( avar :: AVAR, ref :: REF, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE | e ) Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit
