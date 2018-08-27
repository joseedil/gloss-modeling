-- Bouncing ball example

module Main
  ( main
  ) where

import Ball
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- | Ball model
ball :: Ball
ball = Ball (0,fromIntegral dispHeight) (0,0) (0,0) 10

-- | Gravity
gravity :: Vector
gravity = (0,-0.01)

-- | Wind
wind :: Vector
wind = (0.001,0)

-- | Move the ball.
moveBall :: ViewPort -> Float -> Ball -> Ball
moveBall _ _ = (checkBouncing (fromIntegral dispWidth) (fromIntegral dispHeight)) . updateBall . (applyForce gravity) . (applyForce wind)

-- | Display width.
dispWidth :: Int
dispWidth = 640

-- | Display height.
dispHeight :: Int
dispHeight = 360


-- | Translate Viewport origin to the left-bottom extreme of the window.
renderWorld :: Ball -> Picture
renderWorld =
  let
    view = ViewPort
           ((-1)*(fromIntegral $ dispWidth)/2
           ,(-1)*(fromIntegral $ dispHeight)/2)
           0
           1
  in
    applyViewPortToPicture view . render

-- | Simulation
main :: IO ()
main = simulate
  (InWindow
    "Nice Window"               -- Window title
    (dispWidth,dispHeight)      -- Window size
    (0, 0))                     -- Window position
  white                         -- Window background
  300                           -- Time resolution
  ball                          -- Initial model
  renderWorld                   -- Model render function
  moveBall                      -- Model update function
