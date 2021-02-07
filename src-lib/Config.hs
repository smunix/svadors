{-# LANGUAGE RecordWildCards #-}

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
      _fps :: Int,
      _assetsPath :: String
    } ->
    Config
  deriving (Show)

makeLenses ''Config

config :: Config
config = Config {..}
  where
    _pos = (100, 100)
    _size = (960, 640)
    _background = greyN 0.1
    _window =
      InWindow
        "Space Invader Game for Kayla"
        (config ^. size)
        (config ^. pos)
    _fps = 60
    _assetsPath = "./assets/images/"
