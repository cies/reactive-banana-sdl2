-- | Types used for events
module Reactive.Banana.SDL2.Types (
    EventSource
  , SDLEventSource (..)
  , WrappedEvent
  , TickEvent
  ) where

import           Data.Word
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (AddHandler)
import qualified SDL                        as SDL


-- | Generic Event Source
type EventSource a = (AddHandler a, a -> IO ())

-- | An event containing a list of SDL event
type WrappedEvent = R.Event SDL.EventPayload

-- | SDL Tick event
type TickEvent = R.Event Word32

-- | SDL Event Source
data SDLEventSource = SDLEventSource { getSDLEvent  :: EventSource SDL.EventPayload
                                     , getTickEvent :: EventSource Word32 }
