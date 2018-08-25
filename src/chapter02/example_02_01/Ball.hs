module Ball where

import Graphics.Gloss
import Graphics.Gloss.Data.Point.Arithmetic as P
--import Graphics.Gloss.Data.Vector

data Ball = Ball
            !Vector          -- ^ Position
            !Vector          -- ^ Velocity
            !Vector          -- ^ Acceleration
            Float           -- ^ Mass
            deriving Show

-- | 'render' a Ball object.
render :: Ball -> Picture
render (Ball (x,y) _ _ r) =
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
applyForce :: Vector -> Ball -> Ball
applyForce force (Ball pos speed accel r) =
  Ball pos speed (accel P.+ force)  r

updateBall :: Ball -> Ball
updateBall (Ball pos speed accel r) =
  Ball
  (pos P.+ speed P.+ accel)
  (speed P.+ accel)
  (0,0) r

-- | Check is the ball bounced on the border of the screen.
checkBouncing :: Int -> Int -> Ball -> Ball
checkBouncing dispWidth dispHeight (Ball (x,y) (xspeed,yspeed) accel r)
  | ((x > fromIntegral dispWidth) || (x < 0))
  = Ball (x,y) ((-1) Prelude.* xspeed,yspeed) accel r
  | ((y > fromIntegral dispHeight) || (y < 0))
  = Ball (x,y) (xspeed,(-1) Prelude.* yspeed) accel r
  | otherwise = Ball (x,y) (xspeed,yspeed) accel r
