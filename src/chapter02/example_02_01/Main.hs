-- Bouncing ball example

module Main
  ( main
  ) where

import Ball
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- | Ball model
ball :: Ball
ball = Ball (30,330) (0,0) (0,0) 10

-- | Gravity
gravity :: Vector
gravity = (0,-0.1)

-- | Wind
wind :: Vector
wind = (0.01,0)

-- | Move the ball.
moveBall :: ViewPort -> Float -> Ball -> Ball
moveBall _ _ = (checkBouncing dispWidth dispHeight) . updateBall . (applyForce gravity) . (applyForce wind)

-- | Display width.
dispWidth :: Int
dispWidth = 640

-- | Display height.
dispHeight :: Int
dispHeight = 360

-- | Setup window.
window :: Display
window = InWindow "Nice Window" (dispWidth,dispHeight) (0, 0)

-- | Background color
background :: Color
background = white

-- | Translate Viewport origin to the left-bottom extreme of the window.
view :: ViewPort
view = ViewPort
  ((-1)*(fromIntegral $ dispWidth)/2, (-1)*(fromIntegral $ dispHeight)/2)
  0
  1

-- | Simulation
main :: IO ()
main = simulate window background 100 ball (applyViewPortToPicture view . render) moveBall
