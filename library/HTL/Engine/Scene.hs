module HTL.Engine.Scene where

data Scene
  = Scene'Combat
  | Scene'Menu
  | Scene'GameOver
  | Scene'Quit
  | Scene'Victory
  deriving (Show, Eq)