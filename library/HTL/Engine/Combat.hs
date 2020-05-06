{-# LANGUAGE TemplateHaskell #-}
module HTL.Engine.Combat where

import qualified Animate
import SDL.Vect
import GHC.Int
import Control.Lens

import HTL.Engine.Crew
import HTL.Engine.Floor
import HTL.Engine.Types

data CombatVars = CombatVars
  { cvHull :: Float
  , cvEnemyHull :: Float
  , cvWeaponSelected :: Bool
  , cvLastMousePos :: Point V2 Int32
  , cvCrewStates :: [CrewState]
  , cvCrewAnims :: [Animate.Position CrewKey Seconds]
  } deriving (Show, Eq)

makeClassy ''CombatVars

initCombatVars :: CombatVars
initCombatVars = CombatVars
  { cvHull = 1
  , cvEnemyHull = 1
  , cvWeaponSelected = False
  , cvLastMousePos = P (V2 0 0)
  , cvCrewStates = 
    [ initCrewState floorKestrel (5,2)
    , initCrewState floorKestrel (8,3)
    , initCrewState floorKestrel (14,2)
    ] -- testing three crew members
  , cvCrewAnims =
    [ Animate.initPosition CrewKey'Idle
    , Animate.initPosition CrewKey'Idle
    , Animate.initPosition CrewKey'Idle
    ]
  }