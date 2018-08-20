-- Bouncing ball example

module Main
  ( main
  ) where

import Ball
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

--
-- Ball model
xspeed = 1:: Float
yspeed = 3 :: Float

ball = Ball (0,0) (xspeed,yspeed) 15


moveBall :: ViewPort -> Float -> Ball -> Ball
moveBall _ _ b = checkBouncing $ move b
  where
    checkBouncing (Ball (x,y) (xspeed,yspeed) r)
      | ((x > fromIntegral dispWidth) || (x < 0))
      = Ball (x,y) ((-1)*xspeed,yspeed) r
      | ((y > fromIntegral dispHeight) || (y < 0))
      = Ball (x,y) (xspeed,(-1)*yspeed) r
      | otherwise = Ball (x,y) (xspeed,yspeed) r

--
-- Display setup
dispWidth :: Int
dispWidth = 640

dispHeight :: Int
dispHeight = 360

window :: Display
window = InWindow "Nice Window" (dispWidth,dispHeight) (0, 0)

background :: Color
background = white

-- Translate the Viewport origin to the left-bottom extreme of the window.
view :: ViewPort
view = ViewPort
         ((-1)*(fromIntegral $ dispWidth)/2, (-1)*(fromIntegral $ dispHeight)/2)
         0
         1

--
-- Simulation
main :: IO ()
main = simulate window background 60 ball (applyViewPortToPicture view . render) moveBall
