{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.Victory where

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

class Monad m => Victory m where
  victoryStep :: m ()

victoryStep' :: (MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m) => m ()
victoryStep' = do
  input <- getInput
  when (clickAabb (iMouseLeft input) (522, 390) (215, 54)) (toScene Scene'Menu)
  drawVictory

drawVictory :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawVictory = do
  drawStars (0, 0)
  drawVictoryBox (390, 190)
--   drawMark (522, 390) --debug mark
--   drawMark (522 + 215, 390 + 54) --debug mark
  
