module HTL.Resource where

import qualified SDL
import qualified SDL.Image as Image
import qualified Animate
import Data.Text.Conversions (toText)
import Data.StateVar (($=))
import SDL.Vect
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered

import HTL.Engine.Types
import HTL.Engine.Crew

data Resources = Resources
  { rCrewSprites :: Animate.SpriteSheet CrewKey SDL.Texture Seconds
  , rMenuBackgroundSprite :: SDL.Texture
  , rNewGameSprite :: SDL.Texture
  , rQuitSprite :: SDL.Texture
  , rStarSprite :: SDL.Texture 
  , rKestralBaseSprite :: SDL.Texture
  , rKestralFloorSprite :: SDL.Texture
  , rKestralRoomsSprite :: SDL.Texture
  , rEnemyBoxSprite :: SDL.Texture
  , rEnemyShipSprite :: SDL.Texture
  , rHullHealthSprite :: SDL.Texture
  , rHullHealthMaskSprite :: SDL.Texture
  , rFuelCounterSprite :: SDL.Texture
  , rJumpButtonSprite :: SDL.Texture
  , rSubsystemsSprite :: SDL.Texture
  , rSystemsSprite :: SDL.Texture
  , rMarkSprite :: SDL.Texture
  , rGameOverSprite :: SDL.Texture
  }

-- | Produce a new 'SDL.Surface' based on an existing one, but
-- optimized for blitting to the specified 'SDL.PixelFormat'.
convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt)
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing
  surface <$ Raw.freeFormat fmt

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface0 <- Image.load path
  surface <- convertSurface surface0 SDL.RGBA8888
  SDL.freeSurface surface0
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

alphaColorDef :: Animate.Color
alphaColorDef = (0xff,0x00,0xff)

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  menuBackground <- loadTexture "ftl-dats/img/main_menus/main_base2.png" (Just alphaColorDef)
  newGame <- loadTexture "ftl-dats/img/main_menus/start_off.png" (Just alphaColorDef)
  quit <- loadTexture "ftl-dats/img/main_menus/quit_off.png" (Just alphaColorDef)
  stars <- loadTexture "ftl-dats/img/stars/bg_dullstars.png" (Just alphaColorDef)
  kestralBase <- loadTexture "ftl-dats/img/ship/kestral_base.png" (Just alphaColorDef)
  kestralFloor <- loadTexture "ftl-dats/img/ship/kestral_floor.png" (Just alphaColorDef)
  kestralRooms <- loadTexture "ftl-dats/img/ship/floorIconsKestrel.png" (Just alphaColorDef)
  enemyBox  <- loadTexture "ftl-dats/img/combatUI/box_hostiles2.png" (Just alphaColorDef)
  enemyShip <- loadTexture "ftl-dats/img/ship/mantis_cruiser_3_base.png" (Just alphaColorDef)
  hullHealth <- loadTexture "ftl-dats/img/statusUI/top_hull.png" (Just alphaColorDef)
  hullHealthMask <- loadTexture "ftl-dats/img/statusUI/top_hull_bar_mask.png" (Just alphaColorDef)
  fuelCounter <- loadTexture "ftl-dats/img/systemUI/fuel_counter.PNG" (Just alphaColorDef)
  jumpButton <- loadTexture "ftl-dats/img/systemUI/jump_button.PNG" (Just alphaColorDef)
  subsystems <- loadTexture "ftl-dats/img/systemUI/subsystems.PNG" (Just alphaColorDef)
  systems <- loadTexture "ftl-dats/img/systemUI/weapons_systems.png" (Just alphaColorDef)
  mark <- loadTexture "ftl-dats/img/debug_dot.png" (Just alphaColorDef)
  crewSprites <- Animate.readSpriteSheetJSON loadTexture "ftl-dats/anim/crew.json" :: IO (Animate.SpriteSheet CrewKey SDL.Texture Seconds)
  gameOver <- loadTexture "ftl-dats/img/scoreUI/gameover_main.png" (Just alphaColorDef)
  return Resources
    { rMenuBackgroundSprite = menuBackground
    , rNewGameSprite = newGame
    , rQuitSprite = quit
    , rStarSprite = stars
    , rKestralBaseSprite = kestralBase
    , rKestralFloorSprite = kestralFloor
    , rKestralRoomsSprite = kestralRooms
    , rEnemyBoxSprite = enemyBox
    , rEnemyShipSprite = enemyShip
    , rHullHealthSprite = hullHealth
    , rHullHealthMaskSprite = hullHealthMask
    , rFuelCounterSprite = fuelCounter
    , rJumpButtonSprite = jumpButton
    , rSubsystemsSprite = subsystems
    , rSystemsSprite = systems
    , rMarkSprite = mark
    , rCrewSprites = crewSprites
    , rGameOverSprite = gameOver
    }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
    loadTexture path c = SDL.createTextureFromSurface renderer =<< loadSurface path c

freeResources :: Resources -> IO ()
freeResources r = do
  SDL.destroyTexture (rQuitSprite r)
  SDL.destroyTexture (rNewGameSprite r)
  SDL.destroyTexture (rMenuBackgroundSprite r)
  SDL.destroyTexture (rStarSprite r)
  SDL.destroyTexture (rEnemyBoxSprite r)
  SDL.destroyTexture (rKestralBaseSprite r)
  SDL.destroyTexture (rKestralFloorSprite r)
  SDL.destroyTexture (rKestralRoomsSprite r)
  SDL.destroyTexture (rEnemyShipSprite r)
  SDL.destroyTexture (rHullHealthSprite r)
  SDL.destroyTexture (rHullHealthMaskSprite r)
  SDL.destroyTexture (rFuelCounterSprite r)
  SDL.destroyTexture (rJumpButtonSprite r)
  SDL.destroyTexture (rSubsystemsSprite r)
  SDL.destroyTexture (rSystemsSprite r)
  SDL.destroyTexture (rMarkSprite r)
  SDL.destroyTexture (rGameOverSprite r)
