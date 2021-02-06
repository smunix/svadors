module MyLib (someFunc) where

import qualified Assets as A
import qualified Config as Cfg
import Graphics.Gloss (play)
import Lens.Micro.Platform

someFunc :: IO ()
someFunc = do
  print Cfg.def
  putStrLn "someFunc"
  let assets :: A.Assets
      assets = error "nyi"
      cfg :: Cfg.Config
      cfg = Cfg.def
  play
    (cfg ^. Cfg.window)
    (cfg ^. Cfg.background)
    (cfg ^. Cfg.fps)
    ()
    (\_ -> mempty)
    (\e w -> w)
    (\dt w -> w)
