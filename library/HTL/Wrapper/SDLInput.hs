module HTL.Wrapper.SDLInput where

import SDL.Vect
import GHC.Int
import qualified SDL
import Control.Monad.IO.Class (MonadIO(..))

keycodePressed :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodePressed keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Pressed &&
    not keyboardEventRepeat
  _ -> False

keycodeReleased :: SDL.Keycode -> SDL.EventPayload -> Bool
keycodeReleased keycode event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat } ->
    code == keycode &&
    motion == SDL.Released &&
    not keyboardEventRepeat
  _ -> False

mouseClick :: SDL.MouseButton -> SDL.EventPayload -> Maybe (Point V2 Int32)
mouseClick mousebutton event = case event of
  SDL.MouseButtonEvent SDL.MouseButtonEventData{mouseButtonEventButton = button, mouseButtonEventMotion = motion, mouseButtonEventPos = pos} ->
    if button == mousebutton && motion == SDL.Pressed
    then Just pos
    else Nothing
  _ -> Nothing

class Monad m => SDLInput m where
  pollEventPayloads :: m [SDL.EventPayload]

pollEventPayloads' :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads' = liftIO $ map SDL.eventPayload <$> SDL.pollEvents
