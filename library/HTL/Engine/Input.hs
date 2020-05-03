module HTL.Engine.Input where

import SDL.Vect
import GHC.Int
import qualified SDL
import KeyState

data Input = Input
  { iSpace :: KeyState Int
  , iQuit :: Bool
  , iMouseLeft :: Maybe (Point V2 Int32)
  , iMouseRight :: Maybe (Point V2 Int32)
  } deriving (Show, Eq)

initInput :: Input
initInput = Input initKeyState False Nothing Nothing
