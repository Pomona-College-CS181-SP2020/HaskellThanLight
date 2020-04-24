{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.Combat where

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

class Monad m => Combat m where
  combatStep :: m ()

combatStep' :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
combatStep' = do
  drawCombat

drawCombat :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawCombat = do
  drawStars (0, 0)
  drawKestral (16 * 8, 16 * 10)
  drawKestralFloor (16 * 10 + 13, 16 * 16 - 2)
  drawKestralRooms (16 * 11 + 10, 16 * 16 + 7)
  drawEnemyBox (16 * 54, 16 * 2)
  drawEnemyShip (16 * 56, 16 * 4)