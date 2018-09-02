module Liquid where

import Config

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

type Position = Vector
type Size = Vector
type DragIndex = Float

data Liquid = Liquid
              !Position         -- Position
              !Size
              DragIndex
  deriving Show

renderLiquid :: Liquid -> Picture
renderLiquid (Liquid (x,y) (w,h) c) =
  let
    liquid = pictures [color liquidFillColor $ translate (w/2) (h/2) $ rectangleSolid w h]
  in
    translate x y $ liquid
