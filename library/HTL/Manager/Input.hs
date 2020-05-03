module HTL.Manager.Input where

import SDL.Vect
import GHC.Int
import qualified SDL
import Control.Lens
import Control.Monad.State
import KeyState
import Data.Maybe
import Data.List

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
stepControl events Input{iSpace} = Input
  { iSpace = next 1 [SDL.KeycodeSpace] iSpace
  , iQuit = elem SDL.QuitEvent events
  , iMouseLeft = case find isJust $ map (mouseClick SDL.ButtonLeft) events of
                      Just pos -> pos
                      Nothing -> Nothing
  , iMouseRight = case find isJust $ map (mouseClick SDL.ButtonRight) events of
                       Just pos -> pos
                       Nothing -> Nothing
  }
  where
    next count keycodes keystate
      | or $ map pressed keycodes = pressedKeyState
      | or $ map released keycodes = releasedKeyState
      | otherwise = maintainKeyState count keystate
    released keycode = or $ map (keycodeReleased keycode) events
    pressed keycode = or $ map (keycodePressed keycode) events

-- check if mouse click was within specified boundaries (rectangles only)
clickAabb :: Maybe (Point V2 Int32) -> (Int32,Int32) -> (Int32,Int32) -> Bool
clickAabb Nothing _ _ = False
clickAabb (Just pos) (pX,pY) (width,height) =
  if pos ^._y >= pY && pos ^._y <= pY + height &&
     pos ^._x >= pX && pos ^._x <= pX + width
  then True
  else False