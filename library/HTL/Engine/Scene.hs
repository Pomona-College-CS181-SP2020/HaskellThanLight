module HTL.Engine.Scene where

data Scene
  = Scene'Combat
  | Scene'Menu
  | Scene'GameOver
  deriving (Show, Eq)