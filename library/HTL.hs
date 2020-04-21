module HTL
  ( main
  ) where

import qualified SDL
import qualified Data.Text.IO as T

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)
import SDL.Vect
import System.Random

import HTL.Config
import HTL.Effect.Camera
import HTL.Effect.Clock
import HTL.Effect.Logger
import HTL.Effect.Renderer
import HTL.Wrapper.SDLRenderer
import HTL.Manager.Scene
import HTL.Resource
import HTL.Runner
import HTL.Scene.Title
import HTL.State

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  window <- SDL.createWindow "Haskell Than Light" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        }
  runHTL cfg initVars mainLoop
  SDL.destroyWindow window
  freeResources resources
  SDL.quit

newtype HTL a = HTL (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runHTL :: Config -> Vars -> HTL a -> IO a
runHTL config v (HTL m) = evalStateT (runReaderT m config) v

instance Clock HTL where
  delayMilliseconds = liftIO . delayMilliseconds'

instance Logger HTL where
  logText = liftIO . T.putStrLn

instance SDLRenderer HTL where
  drawTexture = drawTexture'
  presentRenderer = presentRenderer'
  clearRenderer = clearRenderer'
  queryTexture = queryTexture'

instance SceneManager HTL where
  toScene = toScene'

instance Renderer HTL where
  clearScreen = clearScreen'
  drawScreen = drawScreen'
  drawGround = drawTextureSprite (rGroundSprites . cResources)

instance Title HTL where
  titleStep = titleStep'

instance CameraControl HTL where
  adjustCamera = adjustCamera'
  disableZoom = disableZoom'
  enableZoom = enableZoom'
