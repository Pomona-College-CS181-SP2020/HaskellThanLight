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
import HTL.Engine.Combat
import HTL.Engine.Frame
import HTL.Engine.Input
import HTL.Manager.Input
import HTL.Manager.Scene

class Monad m => Combat m where
  combatStep :: m ()

combatStep' :: (HasCombatVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m) => m ()
combatStep' = do
  input <- getInput
  -- for testing gameover screen
  when (clickAabb (iMouseLeft input) (0,0) (200,200)) (toScene Scene'GameOver)
  drawCombat
  when (clickAabb (iMouseLeft input) (304, 614) (96, 40)) (weaponSelected)
  when (clickAabb (iMouseLeft input) (928, 96) (288, 448)) (weaponFired)
  

drawCombat :: Renderer m => m ()
drawCombat = do
  drawStars (0, 0)
  drawKestral (16 * 8, 16 * 10)
  drawKestralFloor (16 * 10 + 13, 16 * 16 - 2)
  drawKestralRooms (16 * 11 + 10, 16 * 16 + 7)
  drawEnemyBox (16 * 54, 16 * 2)
  drawEnemyShip (16 * 56, 16 * 4)
  drawHullHealth (16 * 4, 16 * 2)
  drawHullHealthMask (16 * 4 + 10, 16 * 2)
  drawFuelCounter (16 * 23 + 10, 16 * 5)
  drawJumpButton (16 * 30, 16 * 3)
  drawSubsystems (16 * 35, 16 * 37 + 8)
  drawSystems (16 * 4, 16 * 37 + 12)

modifyCombatVars :: (MonadState s m, HasCombatVars s) => (CombatVars -> CombatVars) -> m ()
modifyCombatVars f = modify $ combatVars %~ f
  
weaponSelected :: (MonadState s m, HasCombatVars s) => m ()
weaponSelected = do
  modifyCombatVars $ \cv -> cv { cvWeaponSelected = True }

weaponFired :: (MonadState s m, HasCombatVars s) => m ()
weaponFired = do
  CombatVars{cvHull, cvWeaponSelected, cvCrewAnim} <- gets (view combatVars)
  when cvWeaponSelected (shootEnemy)  

shootEnemy :: (MonadState s m, HasCombatVars s) => m ()
shootEnemy = do
  --damage enemy ship
  modifyCombatVars $ \cv -> cv { cvWeaponSelected = False }