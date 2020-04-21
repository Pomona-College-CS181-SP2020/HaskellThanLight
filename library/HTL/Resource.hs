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
  { rGroundSprites :: SDL.Texture
  -- , rShipBaseSprites :: SDL.Texture
  -- , rShipRoomSprites :: SDL.Texture
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
  ground <- loadTexture "ftl-dats/img/ship/kestral_base.png" (Just alphaColorDef)
  return Resources
    { rGroundSprites = ground
    }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
    loadTexture path c = SDL.createTextureFromSurface renderer =<< loadSurface path c

freeResources :: Resources -> IO ()
freeResources r = do
  SDL.destroyTexture (rGroundSprites r)
