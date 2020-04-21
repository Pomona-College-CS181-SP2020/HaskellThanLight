{-# LANGUAGE TemplateHaskell #-}
module HTL.Runner where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (MonadState(..), modify, gets)
import Control.Monad.Reader (MonadReader(..))
import KeyState

import HTL.Config
import HTL.Effect.Camera
import HTL.Effect.Clock
import HTL.Effect.Logger
import HTL.Effect.Renderer
import HTL.Engine.Camera
import HTL.Engine.Frame
import HTL.Manager.Scene
import HTL.Scene.Title

import HTL.State

titleTransition :: (MonadState a m, CameraControl m) => m ()
titleTransition = do
  adjustCamera initCamera

toScene' :: MonadState Vars m => Scene -> m ()
toScene' scene = modify (\v -> v { vNextScene = scene })

mainLoop ::
  ( MonadReader Config m
  , MonadState Vars m
  , Logger m
  , Clock m
  , CameraControl m
  , Renderer m
  , Title m
  ) => m ()
mainLoop = do
  clearScreen
  scene <- gets vScene
  step scene
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  stepScene scene nextScene
  let quit = False
  unless quit mainLoop
  where

    step scene = do
      case scene of
        Scene'Title -> titleStep

    stepScene scene nextScene = do
      when (nextScene /= scene) $ do
        case nextScene of
          Scene'Title -> titleTransition
        modify (\v -> v { vScene = nextScene })
