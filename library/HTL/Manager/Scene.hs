module HTL.Manager.Scene
  ( SceneManager(..)
  , Scene(..)
  ) where

import HTL.Engine.Scene

class Monad m => SceneManager m where
  toScene :: Scene -> m ()
