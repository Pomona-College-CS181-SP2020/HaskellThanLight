{-# LANGUAGE TemplateHaskell #-}
module HTL.Scene.Combat where

import qualified Animate
import SDL.Vect
import GHC.Int
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify, gets)
import KeyState
import qualified SDL

import HTL.Config
import HTL.Effect.Renderer
import HTL.Engine.Combat
import HTL.Engine.Frame
import HTL.Engine.Input
import HTL.Engine.Types
import HTL.Engine.Floor
import HTL.Engine.Crew
import HTL.Manager.Input
import HTL.Manager.Scene

class Monad m => Combat m where
  combatStep :: m ()

combatStep' :: (HasCombatVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, Control.Monad.IO.Class.MonadIO m) => m ()
combatStep' = do
  input <- getInput
  updateScene
  updateCombat
  drawCombat
  updateCursor (iMousePos input)

modifyCombatVars :: (MonadState s m, HasCombatVars s) => (CombatVars -> CombatVars) -> m ()
modifyCombatVars f = modify $ combatVars %~ f

updateScene ::  (HasCombatVars s, MonadReader Config m, MonadState s m, Renderer m, HasInput m, SceneManager m, Control.Monad.IO.Class.MonadIO m) => m ()
updateScene = do
  input <- getInput
  -- for testing gameover screen
  when (clickAabb (iMouseLeft input) (0,0) (200,200)) (toScene Scene'GameOver)
  when (clickAabb (iMouseLeft input) (304, 614) (96, 40)) (weaponSelected)
  when (clickAabb (iMouseLeft input) (928, 96) (288, 448)) (weaponFired)
  when (clickAabb (iMouseRight input) (0, 0) (1280, 720)) (deselect)

updateCombat :: (HasCombatVars s, MonadState s m, Renderer m, HasInput m) => m ()
updateCombat = do
  input <- getInput
  -- crew update
  CombatVars{cvCrewStates} <- gets (view combatVars)
  let targetTile = findTileByPosition floorKestrel (iMouseRight input)
  -- let cActions = map stepCrewAction targetTile cvCrewStates
  let newCrewStates = map (stepCrewState (iMouseLeft input) targetTile) cvCrewStates
  -- when (newCrewStates == cvCrewStates) (drawJumpButton (200, 200))
  modifyCombatVars $ \cv -> cv { cvCrewStates = newCrewStates }

drawCombat :: (MonadState s m, HasCombatVars s, Renderer m) => m ()
drawCombat = do
  -- textures
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
  -- crew
  CombatVars{cvCrewStates, cvCrewAnims} <- gets (view combatVars)
  mapM_ drawEachCrew $ zip cvCrewStates cvCrewAnims

drawEachCrew :: Renderer m => (CrewState, Animate.Position CrewKey Seconds) -> m ()
drawEachCrew (cState, cAnim) = do
  crewAnimations <- getCrewAnimations
  let crewLoc = Animate.currentLocation crewAnimations cAnim
  drawCrew crewLoc $ csPos cState

-- updateEachCrew :: (HasCombatVars s, MonadState s m, Renderer m, HasInput m) => CrewState -> m ()
-- updateCrew cState = do 
--   input <- getInput
--   let crewSelected = crewAabb (iMouseLeft input) cState

updateCursor :: (MonadState s m, HasCombatVars s, Renderer m, HasInput m) => Maybe (Point V2 Int32) -> m ()
updateCursor Nothing = do
  CombatVars{cvHull, cvWeaponSelected, cvLastMousePos, cvCrewAnims} <- gets (view combatVars)
  when cvWeaponSelected (drawWeaponPointer cvLastMousePos)
updateCursor (Just pos) = do 
  CombatVars{cvHull, cvWeaponSelected, cvLastMousePos, cvCrewAnims} <- gets (view combatVars)
  when cvWeaponSelected (drawWeaponPointer pos)
  modifyCombatVars $ \cv -> cv { cvLastMousePos = pos }

drawWeaponPointer :: (MonadState s m, HasCombatVars s, Renderer m, HasInput m) => Point V2 Int32 -> m ()
drawWeaponPointer pos = do
  let x = fromIntegral (pos ^._x)
  let y = fromIntegral (pos ^._y)
  drawWpnPtr (x, y)

weaponSelected :: (MonadState s m, HasCombatVars s, Control.Monad.IO.Class.MonadIO m) => m ()
weaponSelected = do
  modifyCombatVars $ \cv -> cv { cvWeaponSelected = True }
  SDL.cursorVisible SDL.$= False

deselect :: (MonadState s m, HasCombatVars s, Control.Monad.IO.Class.MonadIO m) => m ()
deselect = do
  modifyCombatVars $ \cv -> cv { cvWeaponSelected = False }
  SDL.cursorVisible SDL.$= True

weaponFired :: (MonadState s m, HasCombatVars s, Control.Monad.IO.Class.MonadIO m) => m ()
weaponFired = do
  CombatVars{cvHull, cvWeaponSelected, cvCrewAnims} <- gets (view combatVars)
  when cvWeaponSelected shootEnemy  

shootEnemy :: (MonadState s m, HasCombatVars s, Control.Monad.IO.Class.MonadIO m) => m ()
shootEnemy = do
  CombatVars{cvEnemyHull} <- gets (view combatVars)
  modifyCombatVars $ \cv -> cv { cvEnemyHull = (clamp (cvEnemyHull-randomDamage) 0 1)}
  deselect

randomDamage :: Float
randomDamage = 0.1 -- make this actually random

--TODO: if right click when weapon selected, then unselect