{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
module Model where

import qualified Assets as A
import qualified Config as Cfg
import Control.Arrow
import Data.Coerce (coerce)
import Graphics.Gloss.Interface.IO.Game
import Lens.Micro.Platform

newtype Time = Time Float
  deriving (Show, Eq)

newtype Pos where
  Pos ::
    { _pos :: (Float, Float)
    } ->
    Pos
  deriving (Show, Eq)

makeLenses ''Pos

class InBound a where
  inBound :: Cfg.Config -> a -> Bool

instance InBound Pos where
  inBound cfg p = check _1 _1 && check _2 _2
    where
      check a b = abs (p ^. pos . a & (/ 2)) <= abs (cfg ^. Cfg.size . b & fromIntegral & (/ 2))

class HasSize a where
  height :: Int
  width :: Int

newtype Offset where
  Offset ::
    { _offset :: (Float, Float)
    } ->
    Offset
  deriving (Show, Eq)

makeLenses ''Offset

data Spaceship where
  L :: Spaceship
  R :: Spaceship
  deriving (Show, Eq)

instance HasSize Spaceship where
  height = 80
  width = 95

data Invader where
  Invader :: Invader
  deriving (Show, Eq)

instance HasSize Invader where
  height = 80
  width = 95

data Bullet
  deriving (Show, Eq)

data WithPos a where
  WithPos ::
    { _withPos :: Pos,
      _object :: a
    } ->
    WithPos a
  deriving (Show, Eq)

makeLenses ''WithPos

instance InBound (WithPos a) where
  inBound cfg = inBound cfg . (^. withPos)

data Model where
  Model ::
    { _spaceship :: WithPos Spaceship,
      _invaders :: [] (WithPos Invader),
      _bullets :: [] (WithPos Bullet)
    } ->
    Model
  deriving (Show, Eq)

makeLenses ''Model

-- | Our initial model
initial :: Cfg.Config -> Model
initial cfg = Model {..}
  where
    _spaceship = WithPos {_withPos = coerce @(Float, Float) (0, -250), _object = L}
    cols, rows :: Int
    (cols, rows) = (10, 3)
    _invaders = do
      x <- [0 .. cols -1]
      y <- [0 .. rows -1]
      let x', y' :: Float
          x' =
            x
              * width @Invader
              - cfg ^. Cfg.size . _1 `div` 2
              + width @Invader `div` 2
                & fromIntegral
          y' =
            cfg ^. Cfg.size . _2 `div` 2
              - y * height @Invader
              & fromIntegral
      return $ WithPos (coerce (x', y')) Invader
    _bullets = []

update :: Cfg.Config -> Float -> Model -> Model
update cfg dt !model =
  model
    & invaders
      . traversed
    %~ move cfg dt model

class Move a b | a -> b where
  move :: Cfg.Config -> b -> Model -> WithPos a -> WithPos a

instance Move Invader Float where
  move cfg dt !model position@(view (withPos . pos) -> (!x, !y)) =
    if inBound cfg position'
      then position'
      else (x, y + dy) & Pos & flip WithPos Invader
    where
      dx = 1
      dy = -81 -- has to be odd number
      movingRight = odd (round y :: Int)
      x' = if movingRight then x + dx else x - dx
      position' = (x', y) & Pos & flip WithPos Invader

instance Move Spaceship Float where
  move cfg dx model position@(view (withPos . pos) -> (!x, !y)) =
    (x + dx, y)
      & Pos
      & flip
        WithPos
        ( if dx >= 0
            then R
            else L
        )

specialKeyDown :: SpecialKey -> Event -> Bool
specialKeyDown sk (EventKey (SpecialKey sk') Down _ _) = sk == sk'
specialKeyDown _ _ = False

events :: Cfg.Config -> Event -> Model -> Model
events cfg (specialKeyDown KeyLeft -> True) model =
  model
    & spaceship
    %~ move cfg (-10) model
events cfg (specialKeyDown KeyRight -> True) model =
  model
    & spaceship
    %~ move cfg (10) model
events cfg ev model = model

render :: Cfg.Config -> A.Assets -> Model -> Picture
render cfg assets model =
  [ renderBackground,
    renderShip,
    renderInvaders
  ]
    & pictures
  where
    renderShip :: Picture
    renderShip =
      ( case model ^. spaceship . object of
          L -> assets ^. A.marioLeft
          R -> assets ^. A.marioRight
      )
        & uncurry
          translate
          ( model
              ^. spaceship
                . withPos
                . pos
          )

    renderBackground :: Picture
    renderBackground = assets ^. A.background

    renderInvaders :: Picture
    renderInvaders =
      model
        ^.. invaders
          . folded
          . withPos
          . pos
        <&> flip (uncurry translate) (assets ^. A.invader)
        & pictures
