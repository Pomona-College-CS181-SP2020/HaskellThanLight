module HTL.Resource where

import qualified SDL
import qualified SDL.Image as Image
import qualified Animate
import Data.Text.Conversions (toText)
import Data.StateVar (($=))
import SDL.Vect
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered

data Resources = Resources
  { rMenuBackgroundSprite :: SDL.Texture
  , rNewGameSprite :: SDL.Texture
  , rQuitSprite :: SDL.Texture
  , rStarSprite :: SDL.Texture 
  , rKestralBaseSprite :: SDL.Texture
  , rKestralFloorSprite :: SDL.Texture
  , rKestralRoomsSprite :: SDL.Texture
  , rEnemyBoxSprite :: SDL.Texture
  , rEnemyShipSprite :: SDL.Texture
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
