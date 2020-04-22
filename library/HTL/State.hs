{-# LANGUAGE TemplateHaskell #-}
module HTL.State where

import Control.Lens

import HTL.Engine.Scene
import HTL.Engine.Input
import HTL.Engine.Camera

data Vars = Vars
  { vScene :: Scene
  , vNextScene :: Scene
  , vInput :: Input
  , vCamera :: Camera
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars Scene'Menu Scene'Menu initInput initCamera

makeClassy ''Vars
