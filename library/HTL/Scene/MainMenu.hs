{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.MainMenu where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState
import Data.Maybe
import SDL.Vect

import HTL.Config
import HTL.Effect.Renderer
import HTL.Engine.Frame
import HTL.Engine.Input
import HTL.Manager.Input
import HTL.Manager.Scene

class Monad m => MainMenu m where
  menuStep :: m ()

menuStep' :: (MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m) => m ()
menuStep' = do
  input <- getInput
  when (clickAabb (iMouseLeft input) (967, 303) (230,32)) (toScene Scene'Combat)
  when (clickAabb (iMouseLeft input) (1090, 370) (115,32)) (toScene Scene'Quit)
  drawMenu

drawMenu :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawMenu = do
  drawMenuBackground (0, 0)
  drawNewGame (16 * 60, 16 * 18)
  -- drawMark (1090, 370) --debug mark
  -- drawMark (1090 + 115, 370 + 32) --debug mark
  drawQuit (16 * 68, 16 * 22)
  
