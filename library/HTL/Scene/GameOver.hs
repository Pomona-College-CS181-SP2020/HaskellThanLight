{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.GameOver where

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

class Monad m => GameOver m where
  gameOverStep :: m ()

gameOverStep' :: (MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m) => m ()
gameOverStep' = do
  input <- getInput
  when (clickAabb (iMouseLeft input) (150, 100) (50,30)) (toScene Scene'Menu)
  drawGameOver

drawGameOver :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawGameOver = do
  drawStars (0, 0)
  drawGameOverBox (390, 190)
  --drawMark (522, 285) --debug mark
  --drawMark (522 + 215, 285 + 54) --debug mark
  
