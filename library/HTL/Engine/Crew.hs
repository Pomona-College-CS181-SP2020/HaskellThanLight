module HTL.Engine.Crew where

import qualified Safe
import qualified Animate
import SDL.Vect
import GHC.Int
import Control.Lens
import Data.Text (Text)
import Data.Maybe
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
    pos = findPositionByTile floor (tileX,tileY)

findPositionByTile :: FloorState -> (Int,Int) -> (Int,Int)
findPositionByTile floor (tileX,tileY) =
  ((fsOffset floor) ^._x + (tileX * 35), (fsOffset floor) ^._y + (tileY * 35))

crewAabb :: Maybe (Point V2 Int32) -> CrewState -> Maybe Bool
crewAabb maybePos cs = case maybePos of
  Just pos -> Just (checkX >= selfX + 6 && checkX < selfX + 29 &&
              checkY >= selfY + 6 && checkY < selfY + 29)
                where 
                  (selfX,selfY) = csPos cs
                  (checkX,checkY) = (fromIntegral $ pos ^._x, fromIntegral $ pos ^._y)
  Nothing -> Nothing

stepCrewAction :: Maybe [(Int,Int)] -> CrewState -> Step CrewAction
stepCrewAction potPath cs = case ca of
  CrewAction'Idle -> if isPath && csSelected cs
    then Step'Change ca CrewAction'Move
    else Step'Sustain ca
  CrewAction'Move -> if isPath && csSelected cs
    then Step'Change ca CrewAction'Move
    else case csMoves cs of
      Just _ -> Step'Sustain CrewAction'Move
      Nothing -> Step'Change ca CrewAction'Idle
  where ca = csAction cs
        isPath = isJust potPath
        

-- stepCrewAction :: Maybe [(Int,Int)] -> CrewState -> Step CrewAction
-- stepCrewAction movesToDo cs = case ca of
--   CrewAction'Idle -> case movesToDo of
--     Just _ -> Step'Change ca CrewAction'Move
--     Nothing -> Step'Sustain ca
--   CrewAction'Move -> case movesToDo of
--     Just _ -> Step'Sustain CrewAction'Move
--     Nothing -> case csMoves cs of
--       Just _ -> Step'Sustain CrewAction'Move
--       Nothing -> Step'Change ca CrewAction'Idle
--   where ca = csAction cs

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
stepCrewState :: Maybe (Point V2 Int32) -> Maybe (Int,Int) -> CrewState -> CrewState
stepCrewState selectClick targetTile cs = case stepCa of
  Step'Change _ ca -> case ca of
    CrewAction'Idle -> CrewState selected ca health (csPos cs) (csCurTile cs) Nothing
    CrewAction'Move -> CrewState selected ca health movePos nextTile movePath
      -- CrewAction'Idle -> CrewState selected ca health movePos nextTile movePath -- start move
      -- CrewAction'Move -> if findPositionByTile (csCurTile cs) == (csPos cs) -- check if done with current step
      --   then CrewState selected ca health movePos nextTile movePath -- regular move
      --   else CrewState selected ca health movePos nextTile movePath -- merge move
      --     where mergePath = Just (csCurTile cs : $ fromJust movePath)
  Step'Sustain ca -> case ca of
    CrewAction'Idle -> CrewState selected ca health (csPos cs) (csCurTile cs) Nothing --stay the same
    CrewAction'Move -> if findPositionByTile floorKestrel (csCurTile cs) == (csPos cs) --check if done with current step
      then case csMoves cs of
        Just [_] -> CrewState selected ca health (csPos cs) (csCurTile cs) Nothing -- all moves over
        Just (_:m:ms) -> CrewState selected ca health nextPos m (Just (m:ms)) -- still moves to do
          where nextPos = moveTowards (csPos cs) (findPositionByTile floorKestrel m)
        _ -> error "should have at least one move in a sustain step."
      else CrewState selected ca health movePos nextTile (csMoves cs)
  where
    movePath = case targetTile of
      Just tT -> findShortestPath floorKestrel (csCurTile cs) tT
      Nothing -> Nothing
    stepCa = stepCrewAction movePath cs
    selected = case crewAabb selectClick cs of
      Just s -> s
      Nothing -> (csSelected cs)
    health = (csHealth cs) -- TODO: Implement taking damage
    nextTile = case movePath of
      Just (t:_) -> t -- step tile to move if move exists
      _ -> (csCurTile cs) -- maintain tile otherwise
    movePos = moveTowards (csPos cs) (findPositionByTile floorKestrel nextTile)

cSpeed :: Int
cSpeed = 1

-- takes: curPos, targetPos
-- return: newPos
moveTowards :: (Int,Int) -> (Int,Int) -> (Int,Int)
moveTowards (cX,cY) (tX,tY) = 
  if dX > 0
  then if dY > 0
    then (intClamp (cX + cSpeed) cX tX, intClamp (cY + cSpeed) cY tY)
    else (intClamp (cX + cSpeed) cX tX, intClamp (cY - cSpeed) tY cY)
  else if dY > 0
    then (intClamp (cX - cSpeed) tX cX, intClamp (cY + cSpeed) cY tY)
    else (intClamp (cX - cSpeed) tX cX, intClamp (cY - cSpeed) tY cY)
  where (dX, dY) = (tX - cX, tY - cY)