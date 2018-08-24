-- Bouncing ball example

module Main
  ( main
  ) where

import Ball
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- | Ball model
xspeed :: Float
xspeed = 1

yspeed :: Float
yspeed = 3

ball :: Ball
ball = Ball (0,0) (xspeed,yspeed) 15

-- | Move the ball.
moveBall :: ViewPort -> Float -> Ball -> Ball
moveBall _ _ = (checkBouncing dispWidth dispHeight) . move

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
main = simulate window background 60 ball (applyViewPortToPicture view . render) moveBall
