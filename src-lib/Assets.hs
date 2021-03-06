{-# LANGUAGE RecordWildCards #-}

-- |
module Assets where

import qualified Config as Cfg
import Graphics.Gloss
import Lens.Micro.Platform

data Assets where
  Assets ::
    { _background :: Picture,
      _spaceship :: Picture,
      _marioLeft :: Picture,
      _marioRight :: Picture,
      _invader :: Picture
    } ->
    Assets

makeLenses ''Assets

assets :: Cfg.Config -> IO Assets
assets cfg = do
  _background <- "galaxy-2643089_960_720.bmp" & path & loadBMP
  _marioLeft <- "mario.left.bmp" & path & loadBMP
  _marioRight <- "mario.right.bmp" & path & loadBMP
  _spaceship <- "mario.bmp" & path & loadBMP
  _invader <- "rstar.bmp" & path & loadBMP
  return Assets {..}
  where
    path :: String -> String
    path = (cfg ^. Cfg.assetsPath <>)
