-- |
module Config where

import Graphics.Gloss (Color, Display (InWindow), greyN)
import Lens.Micro.Platform

data Config where
  Config ::
    { _pos :: (Int, Int),
      _size :: (Int, Int),
      _background :: Color,
      _window :: Display,
      _fps :: Int
    } ->
    Config
  deriving (Show)

makeLenses ''Config

def :: Config
def =
  Config {}
    & pos .~ (100, 100)
    & size .~ (960, 640)
    & background .~ greyN 0.1
    & window .~ InWindow "Space Invader Game for Kayla" (def ^. size) (def ^. pos)
    & fps .~ 60
