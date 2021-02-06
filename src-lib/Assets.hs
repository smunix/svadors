-- |
module Assets where

import Graphics.Gloss
import Lens.Micro.Platform

data Assets where
  Assets ::
    { _background :: Picture,
      _spaceship :: Picture,
      _invader :: Picture
    } ->
    Assets

makeLenses ''Assets
