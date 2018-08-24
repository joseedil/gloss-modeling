module Ball where

import Graphics.Gloss

data Ball = Ball
            Point          -- Center
            Point          -- Velocity
            Float          -- Radius
            deriving Show

-- | 'render' a Ball object.
render :: Ball -> Picture
render (Ball (x,y) (xspeed,yspeed) r) =
  let
    strokeColor :: Color
    strokeColor = greyN 0.3

    fillColor :: Color
    fillColor = withAlpha 0.3 black
  in
    translate x y
    $ pictures [color fillColor $ circleSolid r,
                color strokeColor $ thickCircle r (r/10)]

-- |'move' updates the ball position.
move :: Ball -> Ball
move (Ball (x,y) (xspeed,yspeed) r) =
  Ball (x+xspeed, y+yspeed) (xspeed,yspeed) r

-- | Check is the ball bounced on the border of the screen.
checkBouncing :: Int -> Int -> Ball -> Ball
checkBouncing dispWidth dispHeight (Ball (x,y) (xspeed,yspeed) r)
  | ((x > fromIntegral dispWidth) || (x < 0))
  = Ball (x,y) ((-1)*xspeed,yspeed) r
  | ((y > fromIntegral dispHeight) || (y < 0))
  = Ball (x,y) (xspeed,(-1)*yspeed) r
  | otherwise = Ball (x,y) (xspeed,yspeed) r
