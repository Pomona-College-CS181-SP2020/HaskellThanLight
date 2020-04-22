module HTL.Manager.Input where

import qualified SDL
import Control.Monad.State
import KeyState

import HTL.Engine.Input
import HTL.Wrapper.SDLInput
import HTL.State

class Monad m => HasInput m where
  updateInput :: m ()
  setInput :: Input -> m ()
  getInput :: m Input

updateInput' :: (HasInput m, SDLInput m) => m ()
updateInput' = do
  input <- getInput
  events <- pollEventPayloads
  setInput (stepControl events input)

getInput' :: MonadState Vars m => m Input
getInput' = gets vInput

setInput' :: MonadState Vars m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl events Input{iSpace,iEscape} = Input
  { iSpace = next 1 [SDL.KeycodeSpace] iSpace
  , iEscape = next 1 [SDL.KeycodeEscape] iEscape
  , iQuit = elem SDL.QuitEvent events
  }
  where
    next count keycodes keystate
      | or $ map pressed keycodes = pressedKeyState
      | or $ map released keycodes = releasedKeyState
      | otherwise = maintainKeyState count keystate
    released keycode = or $ map (keycodeReleased keycode) events
    pressed keycode = or $ map (keycodePressed keycode) events
