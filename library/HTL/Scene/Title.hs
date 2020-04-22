{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.Title where

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

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
titleStep' = do
  drawTitle

drawTitle :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawTitle = do
  drawStars (0, 0)
  drawKestral (16 * 8, 16 * 10)
  drawKestralFloor (16 * 10 + 12, 16 * 16)
  drawEnemyBox (16 * 54, 16 * 2)
