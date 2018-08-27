module Ball where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
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
checkBouncing :: Float -> Float -> Ball -> Ball
checkBouncing width height b@(Ball pos@(x,y) (xspeed,yspeed) accel r)
  | (x > width) = Ball (2*width-x, y) ((-1) * xspeed,yspeed) accel r
  | (x < 0) = Ball ((-1)*x,y) ((-1) * xspeed,yspeed) accel r
  | (y > height) = Ball (x, 2*height-y) (xspeed,(-1) * yspeed) accel r
  | (y < 0) = Ball (x,(-1)*y) (xspeed, (-1)*yspeed) accel r
  | otherwise = b
