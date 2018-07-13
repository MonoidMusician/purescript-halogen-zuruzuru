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
  , PageCoord
  , mouseEventToPageCoord
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref

import Data.Maybe (Maybe)

import Web.Event.EventTarget (eventListener, addEventListener, removeEventListener)
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.HTML (window)
import Web.UIEvent.MouseEvent.EventTypes (mousemove, mouseup)
import Web.HTML.Window (toEventTarget) as Window

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

type PageCoord =
  { pageX ∷ Number
  , pageY ∷ Number
  }

dragEventSource
  ∷ ∀ f m
  . MonadAff m
  ⇒ MouseEvent
  → (DragEvent → Maybe (f ES.SubscribeStatus))
  → ES.EventSource f m
dragEventSource mouseEvent = ES.eventSource' \emit → do
  let initEv = mouseEventToPageCoord mouseEvent
  eventRef ← Ref.new initEv
  remover ← Ref.new (pure unit :: Effect Unit)

  mouseMove <- eventListener \ev → do
    prevEv ← Ref.read eventRef
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
    Ref.write ev' eventRef
    emit $ Move (unsafeEventToMouseEvent ev) dragData

  mouseUp <- eventListener \ev → do
    join $ Ref.read remover
    emit $ Done (unsafeEventToMouseEvent ev)

  let
    removeListeners ∷ Effect Unit
    removeListeners = do
      win ← Window.toEventTarget <$> window
      removeEventListener mousemove mouseMove false win
      removeEventListener mouseup mouseUp false win

  Ref.write removeListeners remover

  win ← Window.toEventTarget <$> window
  addEventListener mousemove mouseMove false win
  addEventListener mouseup mouseUp false win
  pure removeListeners

unsafeEventToPageCoord ∷ Event → PageCoord
unsafeEventToPageCoord = unsafeCoerce

unsafeEventToMouseEvent ∷ Event → MouseEvent
unsafeEventToMouseEvent = unsafeCoerce

mouseEventToPageCoord ∷ MouseEvent → PageCoord
mouseEventToPageCoord = unsafeCoerce
