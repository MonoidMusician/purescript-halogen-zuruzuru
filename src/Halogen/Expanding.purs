module Halogen.Expanding
    ( expandingComponent
    , Query(..)
    , main
    ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Comonad (extract)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event, keyboardEventToEvent)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (setValue, setWidth, value)
import DOM.HTML.Types (HTMLInputElement, htmlDocumentToDocument, htmlElementToElement, htmlInputElementToHTMLElement, readHTMLInputElement)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (clientWidth, setAttribute)
import DOM.Node.Node (appendChild, parentNode, removeChild, setTextContent)
import DOM.Node.Types (Element, elementToNode)
import DOM.Util.TextCursor (Direction(..), TextCursor(..), content, empty)
import DOM.Util.TextCursor.Element (TextCursorElement, setTextCursor, textCursor)
import DOM.Util.TextCursor.Element.Type (read')
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Foldable (traverse_)
import Data.Foreign (Foreign)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)

data Query a
  = Set TextCursor a
  | Raising a
  | PreventDefault Event (Query a)
  | NoOp a
  | Update a

foreign import computedStyle :: forall eff. Element -> Eff ( dom :: DOM | eff ) String
foreign import fixStyle :: forall eff. Element -> Eff ( dom :: DOM | eff ) Unit

-- | Closely based on https://stackoverflow.com/a/7168967
testWidth :: forall eff. HTMLInputElement -> String -> Eff ( dom :: DOM | eff ) (Maybe Number)
testWidth input val = do
  tmp <- createElement "div" <<< htmlDocumentToDocument =<< document =<< window
  let tmpNode = elementToNode tmp

  setTextContent val tmpNode

  let inputHTMLElement = htmlInputElementToHTMLElement input
  let inputElement = htmlElementToElement inputHTMLElement
  let inputNode = elementToNode inputElement
  styl <- computedStyle inputElement
  setAttribute "style" styl tmp
  fixStyle tmp

  parentNode inputNode >>= traverse \parNode -> do
    _ <- appendChild tmpNode parNode
    width <- clientWidth tmp
    _ <- removeChild tmpNode parNode
    pure width

type Settings =
  { min :: Number
  , max :: Number
  , padding :: Number
  -- , blurred :: Maybe Number
  }


obtainInput :: Maybe Foreign -> Maybe HTMLInputElement
obtainInput = bindFlipped (readHTMLInputElement >>> runExcept >>> hush)

obtainInputTC :: Maybe Foreign -> Maybe TextCursorElement
obtainInputTC = bindFlipped (read' >>> runExcept >>> hush)

expandingComponent :: forall m eff.
  MonadEff ( dom :: DOM | eff ) m =>
  Settings ->
  H.Component HH.HTML Query TextCursor TextCursor m
expandingComponent settings =
  H.lifecycleComponent
    { initialState: Tuple (ceil settings.min)
    , render
    , eval
    , receiver: HE.input Set
    , initializer: Just (Update unit)
    , finalizer: Nothing
    }
  where
    label = H.RefLabel "textcursor-component" :: H.RefLabel
    render :: Tuple Int TextCursor -> H.ComponentHTML Query
    render (Tuple w v) = HH.input
      [ HP.ref label -- give it a label
      , HP.value (content v) -- set the value
      , HP.attr (AttrName "style") ("width: " <> show w <> "px") -- set the width
      , HE.onInput (HE.input_ Raising) -- notify parent on input events
      , HE.onInput (HE.input_ Raising)
      , HE.onClick (HE.input_ Raising)
      , HE.onKeyUp (HE.input_ Raising)
      , HE.onFocus (HE.input_ Raising)
      -- , HE.onKeyPress (HE.input (\e u -> PreventDefault (keyboardEventToEvent e) (NoOp u)))
      ]

    withEl h = H.getRef label >>= obtainInputTC >>> traverse_ h

    eval :: Query ~> H.ComponentDSL (Tuple Int TextCursor) Query TextCursor m
    -- When an input event occurs, get the value and notify the parent
    eval (NoOp next) = pure next
    eval (PreventDefault e next) = do
      v <- H.gets extract
      withEl \el ->
        H.liftEff $ setTextCursor v el
      eval next
    eval (Raising next) = next <$ do
      v <- H.gets extract
      withEl \el -> do
        v' <- H.liftEff $ textCursor el
        H.liftEff $ setTextCursor v el
        H.raise v'
    -- Set the input value in state and update the size
    eval (Set v next) = do
      withEl \el -> H.liftEff $ setTextCursor v el
      H.modify (_ $> v) *> eval (Update next)
    -- Update the size of the input to correspond to the value it will have
    -- on the next render
    eval (Update next) = next <$ do
      H.getRef label >>= obtainInput >>> traverse_ \el -> do
        v <- H.gets extract
        H.liftEff (testWidth el (content v)) >>= traverse_ \w' -> do
          let
            w = ceil
              $ max settings.min
              $ min settings.max
              $ w' + settings.padding
          H.liftEff $ setWidth w el
          H.modify (lmap (const w))
      pure unit

data DemoQuery a
  = Reset TextCursor a
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
    com = expandingComponent { min: 50.0, max: 300.0, padding: 15.0 }

    update = H.put >>> (_ *> inform)
    inform = do
      TextCursor r <- H.get
      H.liftEff $ log $ unsafeCoerce [r.before, r.selected, r.after]

    render :: TextCursor -> H.ParentHTML DemoQuery Query Unit m
    render s = HH.div_ [HH.slot unit com s (HE.input Receive)]

    eval :: DemoQuery ~> H.ParentDSL TextCursor DemoQuery Query Unit Void m
    eval (Reset v a) = a <$ do
      update v
    eval (Receive v a) = a <$ do
      let ignore = isJust <<< stripPrefix (Pattern "ignore: ") <<< content
      prev <- H.get
      when (not (ignore prev && ignore v)) do
        update v
      when (isJust $ stripSuffix (Pattern "reset") $ content v) do
        eval (Reset empty unit)

main :: forall e. Eff ( avar :: AVAR, ref :: REF, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE | e ) Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit
