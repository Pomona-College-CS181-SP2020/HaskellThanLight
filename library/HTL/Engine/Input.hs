module HTL.Engine.Input where

import KeyState

data Input = Input
  { iSpace :: KeyState Int
  , iEscape :: KeyState Int
  , iQuit :: Bool
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState initKeyState False
