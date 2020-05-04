{-# LANGUAGE TemplateHaskell #-}
module HTL.State where

import Control.Lens

import HTL.Engine.Scene
import HTL.Engine.Combat
import HTL.Engine.Input
import HTL.Engine.Camera

data Vars = Vars
  { vScene :: Scene
  , vNextScene :: Scene
  , vCombat :: CombatVars
  , vInput :: Input
  , vCamera :: Camera
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars Scene'Menu Scene'Menu initCombatVars initInput initCamera

instance HasCombatVars Vars where
  combatVars = lens vCombat (\v s -> v { vCombat = s })

makeClassy ''Vars
