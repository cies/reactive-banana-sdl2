module Reactive.Banana.SDL2 (
    module Reactive.Banana.SDL2.Types
  , module Reactive.Banana.SDL2.Util
  , getSDLEventSource
  , runSDLPump
  , runCappedSDLPump
  ) where

import           Control.Monad
import           Data.Word
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (newAddHandler)
import qualified SDL                        as SDL
import qualified SDL.Raw                    as SDLR

import           Reactive.Banana.SDL2.Types
import           Reactive.Banana.SDL2.Util


getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> newAddHandler <*> newAddHandler

-- | One step in the main event loop, returning False when it needs to stop.
mainSDLPump :: SDLEventSource -> IO Bool
mainSDLPump sdlEventSource = do
  let esdl  = getSDLEvent  sdlEventSource
      etick = getTickEvent sdlEventSource
  tick    <- SDL.ticks
  mEvents <- collectEvents
  case mEvents of Nothing     -> return False
                  Just events -> do mapM (fire esdl) events
                                    fire etick tick
                                    return True

-- | Collect SDL events.
-- Evaluate to Nothing on quit, otherwise evaluates to the last event.
collectEvents :: IO (Maybe [SDL.EventPayload])
collectEvents = do
  e <- SDL.pollEvent
  case fmap SDL.eventPayload e of
    Just SDL.QuitEvent -> return Nothing
    Nothing            -> return (Just [])
    Just event         -> liftM (liftM (event:)) collectEvents

-- | The main event loop.
runSDLPump :: SDLEventSource -> IO ()
runSDLPump sdlEventSource = whileM $ mainSDLPump sdlEventSource

-- | The main event loop, capped at a given FPS.
runCappedSDLPump :: Word16 -> SDLEventSource -> IO ()
runCappedSDLPump fpsCap sdlEventSource = do
  startTick       <- SDL.ticks
  shouldContinue  <- mainSDLPump sdlEventSource
  endTick         <- SDL.ticks
  let ticks        = fromIntegral $ endTick - startTick
      secsPerFrame = fromIntegral $ 1000 `div` fpsCap
  when (ticks < secsPerFrame) $ SDL.delay (secsPerFrame - ticks)
  when shouldContinue         $ runCappedSDLPump fpsCap sdlEventSource

