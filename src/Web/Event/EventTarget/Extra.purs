module Web.Event.EventTarget.Extra
  ( addEventListenerOnce
  ) where

import Prelude
import Effect (Effect)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (EventListener, EventTarget)

foreign import addEventListenerOnce :: EventType -> EventListener -> Boolean -> EventTarget -> Effect Unit
