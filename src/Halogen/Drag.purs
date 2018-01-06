{-
Copyright 2016 SlamData, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Halogen.Component.Utils.Drag
  ( dragEventSource
  , DragData
  , DragEvent(..)
  , DragEffects
  , PageCoord
  , mouseEventToPageCoord
  ) where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)

import Data.Maybe (Maybe)

import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener, addEventListener, removeEventListener)
import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (mousemove, mouseup)
import DOM.HTML.Types (windowToEventTarget)

import Halogen.Query.EventSource as ES

import Unsafe.Coerce (unsafeCoerce)

type DragData =
  { x ∷ Number
  , y ∷ Number
  , deltaX ∷ Number
  , deltaY ∷ Number
  , offsetX ∷ Number
  , offsetY ∷ Number
  }

data DragEvent
  = Move MouseEvent DragData
  | Done MouseEvent

type DragEffects eff =
  ( dom ∷ DOM
  , ref ∷ REF
  , avar ∷ AVAR
  | eff
  )

type PageCoord =
  { pageX ∷ Number
  , pageY ∷ Number
  }

dragEventSource
  ∷ ∀ f m eff
  . MonadAff (DragEffects eff) m
  ⇒ MouseEvent
  → (DragEvent → Maybe (f ES.SubscribeStatus))
  → ES.EventSource f m
dragEventSource mouseEvent = ES.eventSource' \emit → do
  let initEv = mouseEventToPageCoord mouseEvent
  eventRef ← newRef initEv

  let
    removeListeners ∷ Eff (DragEffects eff) Unit
    removeListeners = do
      win ← windowToEventTarget <$> window
      removeEventListener mousemove mouseMove false win
      removeEventListener mouseup mouseUp false win

    mouseMove ∷ EventListener (DragEffects eff)
    mouseMove = eventListener \ev → do
      prevEv ← readRef eventRef
      let
        ev' = unsafeEventToPageCoord ev
        x1 = prevEv.pageX
        y1 = prevEv.pageY
        x2 = ev'.pageX
        y2 = ev'.pageY
        dragData =
          { x: x2
          , y: y2
          , deltaX: x2 - x1
          , deltaY: y2 - y1
          , offsetX: x2 - initEv.pageX
          , offsetY: y2 - initEv.pageY
          }
      writeRef eventRef ev'
      emit $ Move (unsafeEventToMouseEvent ev) dragData

    mouseUp ∷ EventListener (DragEffects eff)
    mouseUp = eventListener \ev → do
      removeListeners
      emit $ Done (unsafeEventToMouseEvent ev)

  win ← windowToEventTarget <$> window
  addEventListener mousemove mouseMove false win
  addEventListener mouseup mouseUp false win
  pure removeListeners

unsafeEventToPageCoord ∷ Event → PageCoord
unsafeEventToPageCoord = unsafeCoerce

unsafeEventToMouseEvent ∷ Event → MouseEvent
unsafeEventToMouseEvent = unsafeCoerce

mouseEventToPageCoord ∷ MouseEvent → PageCoord
mouseEventToPageCoord = unsafeCoerce
