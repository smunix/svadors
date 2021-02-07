module MyLib (someFunc) where

import qualified Assets as A
import qualified Config as Cfg
import Graphics.Gloss (play)
import Lens.Micro.Platform
import Model (events, initial, render, update)

someFunc :: IO ()
someFunc = do
  print Cfg.config
  putStrLn "someFunc"
  let cfg :: Cfg.Config
      cfg = Cfg.config
  assets <- A.assets Cfg.config
  play
    (cfg ^. Cfg.window)
    (cfg ^. Cfg.background)
    (cfg ^. Cfg.fps)
    (initial cfg)
    (render cfg assets)
    (events cfg)
    (update cfg)
