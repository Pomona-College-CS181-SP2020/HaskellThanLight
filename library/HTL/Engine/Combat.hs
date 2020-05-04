{-# LANGUAGE TemplateHaskell #-}
module HTL.Engine.Combat where

import qualified Animate
import Control.Lens

import HTL.Engine.Crew
import HTL.Engine.Types

data CombatVars = CombatVars
  { cvHull :: Float
  , cvWeaponSelected :: Bool
  --, cvCrewState :: CrewState
  , cvCrewAnim :: Animate.Position CrewKey Seconds
  } deriving (Show, Eq)

makeClassy ''CombatVars 

initCombatVars :: CombatVars
initCombatVars = CombatVars
  { cvHull = 1
  , cvWeaponSelected = False
  --, cvCrewState = CrewState False CrewAction'Idle 1 (V2 60 60) [] --Gotta Change stuff here
  , cvCrewAnim = Animate.initPosition CrewKey'Idle
  }