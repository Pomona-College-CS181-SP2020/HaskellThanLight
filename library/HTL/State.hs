{-# LANGUAGE TemplateHaskell #-}
module HTL.State where

import Control.Lens

import HTL.Engine.Scene
import HTL.Engine.Camera

data Vars = Vars
  { vScene :: Scene
  , vNextScene :: Scene
  , vCamera :: Camera
  } deriving (Show, Eq)

initVars :: Vars
initVars = Vars Scene'Title Scene'Title initCamera

makeClassy ''Vars
