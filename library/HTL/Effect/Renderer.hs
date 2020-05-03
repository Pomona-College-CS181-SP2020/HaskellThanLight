module HTL.Effect.Renderer where

import qualified Animate
import qualified SDL
import Data.StateVar (($=))
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader

import HTL.Config
import HTL.Engine.Types
import HTL.Engine.Crew
import HTL.Wrapper.SDLRenderer

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()
  getCrewAnimations :: m (Animations CrewKey)
  drawCrew :: DrawSprite CrewKey m
  drawMenuBackground :: (Int, Int) -> m ()
  drawNewGame :: (Int, Int) -> m ()
  drawQuit :: (Int, Int) -> m ()
  drawStars :: (Int, Int) -> m ()
  drawKestral :: (Int, Int) -> m ()
  drawKestralFloor :: (Int, Int) -> m ()
  drawKestralRooms :: (Int, Int) -> m ()
  drawEnemyBox :: (Int, Int) -> m ()
  drawEnemyShip :: (Int, Int) -> m()
  drawHullHealth :: (Int,Int) -> m()
  drawHullHealthMask :: (Int, Int) -> m()
  drawFuelCounter :: (Int, Int) -> m()
  drawJumpButton :: (Int, Int) -> m()
  drawSubsystems :: (Int, Int) -> m()
  drawSystems :: (Int, Int) -> m()
  drawMark :: (Int,Int) -> m()
  drawGameOverBox :: (Int,Int) -> m()

clearScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
clearScreen' = do
  renderer <- asks cRenderer
  clearRenderer renderer

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  renderer <- asks cRenderer
  presentRenderer renderer

--

drawTextureSprite :: (SDLRenderer m, MonadReader Config m) => (Config -> SDL.Texture) -> (Int, Int) -> m ()
drawTextureSprite getTex (x,y) = do
  renderer <- asks cRenderer
  tex <- asks getTex
  SDL.TextureInfo{textureWidth,textureHeight} <- queryTexture tex
  let dim = V2 textureWidth textureHeight
  drawTexture
    renderer
    tex
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)

drawSprite :: (SDLRenderer m, MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawSprite ss clip (x,y) = do
  renderer <- asks cRenderer
  sheet <- asks (Animate.ssImage . ss)
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  drawTexture
    renderer
    sheet
    (Just clip')
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)

--

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral
