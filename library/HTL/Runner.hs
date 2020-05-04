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
import HTL.Engine.Combat
import HTL.Engine.Frame
import HTL.Engine.Input
import HTL.Manager.Input
import HTL.Manager.Scene
import HTL.Scene.Combat
import HTL.Scene.MainMenu
import HTL.Scene.GameOver

import HTL.State

combatTransition :: (HasCombatVars a, MonadState a m, CameraControl m) => m ()
combatTransition = do
  adjustCamera initCamera
  modify $ combatVars .~ initCombatVars

menuTransition :: (MonadState a m, CameraControl m) => m ()
menuTransition = do
  adjustCamera initCamera

gameOverTransition :: (MonadState a m, CameraControl m) => m ()
gameOverTransition = do
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
  , HasInput m
  , Combat m
  , MainMenu m
  , GameOver m
  ) => m ()
mainLoop = do
  updateInput
  input <- getInput
  clearScreen
  scene <- gets vScene
  step scene
  drawScreen
  delayMilliseconds frameDeltaMilliseconds
  nextScene <- gets vNextScene
  stepScene scene nextScene
  let quit = iQuit input || nextScene == Scene'Quit
  unless quit mainLoop
  where

    step scene = do
      case scene of
        Scene'Combat -> combatStep
        Scene'Menu -> menuStep
        Scene'GameOver -> gameOverStep
        Scene'Quit -> return ()

    stepScene scene nextScene = do
      when (nextScene /= scene) $ do
        case nextScene of
          Scene'Combat -> combatTransition
          Scene'Menu -> menuTransition
          Scene'GameOver -> gameOverTransition
          Scene'Quit -> return ()
        modify (\v -> v { vScene = nextScene })
