{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.MainMenu where

import qualified Animate
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState

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
  when (ksStatus (iSpace input) == KeyStatus'Pressed) (toScene Scene'Combat)
  drawMenu

drawMenu :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawMenu = do
  drawMenuBackground (0, 0)
  drawNewGame (16 * 60, 16 * 18)
  drawQuit (16 * 68, 16 * 22)
  
