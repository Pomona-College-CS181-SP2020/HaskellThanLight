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
import HTL.Manager.Scene

class Monad m => Title m where
  titleStep :: m ()

titleStep' :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
titleStep' = do
  drawTitle

drawTitle :: (MonadReader Config m, MonadState s m, Renderer m, SceneManager m) => m ()
drawTitle = do
  drawGround (0, groundY)
