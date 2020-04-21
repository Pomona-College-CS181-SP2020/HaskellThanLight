module HTL.Config
  ( Config(..)
  , Resources(..)
  ) where

import qualified SDL

import HTL.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
