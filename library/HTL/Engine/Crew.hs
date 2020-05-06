module HTL.Engine.Crew where

import qualified Safe
import qualified Animate
import SDL.Vect
import GHC.Int
import Control.Lens
import Data.Text (Text)
import Linear (V2(..))

import HTL.Engine.Camera
import HTL.Engine.Frame
import HTL.Engine.Types
import HTL.Engine.Step
import HTL.Engine.Floor

data CrewAction
  = CrewAction'Idle
  | CrewAction'Move
  deriving (Show, Eq)

data CrewState = CrewState
  { csSelected :: Bool
  , csAction :: CrewAction
  , csHealth :: Percent
  , csPos :: (Int,Int)
  , csCurTile :: (Int,Int)
  , csMoves :: Maybe [(Int,Int)]
  } deriving (Show, Eq)

data CrewKey
  = CrewKey'Idle
  | CrewKey'Up
  | CrewKey'Down
  | CrewKey'Right
  | CrewKey'Left
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName CrewKey where
  keyName = crewKey'keyName

crewKey'keyName :: CrewKey -> Text
crewKey'keyName = \case
  CrewKey'Idle -> "Idle"
  CrewKey'Up -> "Up"
  CrewKey'Down -> "Down"
  CrewKey'Right -> "Right"
  CrewKey'Left -> "Left"

initCrewState :: FloorState -> (Int,Int) -> CrewState
initCrewState floor (tileX,tileY) =
  CrewState False CrewAction'Idle 1 pos (tileX,tileY) Nothing
  where
    pos = ((fsOffset floor) ^._x + (tileX * 35), (fsOffset floor) ^._y + (tileY * 35))

crewAabb :: Maybe (Point V2 Int32) -> CrewState -> Bool
crewAabb maybePos cs = case maybePos of
  Just pos -> checkX >= selfX + 6 && checkX < selfX + 29 &&
              checkY >= selfY + 6 && checkY < selfY + 29
                where 
                  (selfX,selfY) = csPos cs
                  (checkX,checkY) = (fromIntegral $ pos ^._x, fromIntegral $ pos ^._y)
  Nothing -> False

stepCrewAction :: Maybe [(Int,Int)] -> CrewState -> Step CrewAction
stepCrewAction movesToDo cs = case ca of
  CrewAction'Idle -> case movesToDo of
    Just _ -> Step'Change ca CrewAction'Move
    Nothing -> Step'Sustain ca
  CrewAction'Move -> case movesToDo of
    Just _ -> Step'Sustain CrewAction'Move
    Nothing -> case csMoves cs of
      Just _ -> Step'Sustain CrewAction'Move
      Nothing -> Step'Change ca CrewAction'Idle
  where ca = csAction cs

-- stepCrewAction :: Input -> CrewState -> Step CrewAction
-- stepCrewAction input cs = case ca of
--   CrewAction'Idle -> case csMoves cs of
--     Just csm -> Step'Change ca CrewAction'Move
--     Nothing -> Step'Sustain CrewAction'Idle
--   CrewAction'Move -> case csMoves cs of
--     Just csm -> Step'Sustain CrewAction'Move
--     Nothing -> Step'Change ca CrewAction'Idle
--   where ca = csAction cs

-- args: change in action, if selected on frame, current state
-- returns: updated state
-- stepCrewState :: Step CrewAction -> Bool -> CrewState -> CrewState
-- stepCrewState stepCa slct cs = case stepCa of
--   Step'Change _ ca -> case ca of
--     CrewAction'Idle -> --stop moving (empty move list)
--     CrewAction'Move -> --move towards first move
--   Step'Sustain ca -> case ca of
--     CrewAction'Idle -> cs --stay the same
--     CrewAction'Move -> case csMoves cs of --check if move is over
--       Just (m:ms) -> if csPos cs == m
--                      then CrewState selected () --move over
--                      else --move not over
--       Nothing -> --
--   where
--     selected = slct
--     nextAction
--     health
--     pos
--     curtile
--     movesToDo
