{-# LANGUAGE TemplateHaskell #-}
module HTL.Engine.Combat where

import qualified Animate
import SDL.Vect
import GHC.Int
import Control.Lens

import HTL.Engine.Crew
import HTL.Engine.Types

data CombatVars = CombatVars
  { cvHull :: Float
  , cvWeaponSelected :: Bool
  , cvLastMousePos :: Point V2 Int32
  --, cvCrewState :: CrewState
  , cvCrewAnim :: Animate.Position CrewKey Seconds
  } deriving (Show, Eq)

makeClassy ''CombatVars 

initCombatVars :: CombatVars
initCombatVars = CombatVars
  { cvHull = 1
  , cvWeaponSelected = False
  , cvLastMousePos = P (V2 0 0)
  --, cvCrewState = CrewState False CrewAction'Idle 1 (V2 60 60) [] --Gotta Change stuff here
  , cvCrewAnim = Animate.initPosition CrewKey'Idle
  }