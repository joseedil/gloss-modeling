module Ball where

import Config

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector


type Index = Int
type Position = Vector
type Velocity = Vector
type Acceleration = Vector
type Mass = Float

data Ball = Ball
            !Index
            !Position          -- ^ Position
            !Velocity          -- ^ Velocity
            !Acceleration      -- ^ Acceleration
            Mass               -- ^ Mass
            deriving Show

-- Instances --
-- Balls are equal if they have the same index.
instance Eq Ball where
  b1 == b2 = ballIndex b1 == ballIndex b2

-- Order balls by their indices.
instance Ord Ball where
  compare b1 b2 = compare (ballIndex b1) (ballIndex b2)

-- Take the ball index
ballIndex :: Ball -> Index
ballIndex (Ball i _ _ _ _) = i

-- Set ball index
ballSetIndex :: Index -> Ball -> Ball
ballSetIndex i (Ball _ pos vel accel m) =
  Ball i pos vel accel m


-- Rendering --
nullVector :: Vector
nullVector = (0,0)

origin :: Vector
origin = nullVector

-- | Render a Ball object.
renderBall :: Ball -> Picture
renderBall (Ball _ (x,y) v _ m) =
  let
    ball = pictures [color beadFillColor $ circleSolid m,
                     color beadStrokeColor $ thickCircle m (m/10)]
    vel = if showBeadVelocity
          then Color velVectorColor $ Line [origin, mulSV 10 v]
          else Blank
  in
    translate x y $ pictures [ball, vel]


-- Movement --
-- |'move' updates the ball position.
applyForce :: Vector -> Ball -> Ball
applyForce force (Ball i pos speed accel m) =
  Ball i pos speed (accel P.+ ((1/m) P.* force)) m

updateBall :: Ball -> Ball
updateBall (Ball i pos speed accel r) =
  Ball
  i
  (pos P.+ speed P.+ accel)
  (speed P.+ accel)
  nullVector
  r

-- | Check is the ball bounced on the border of the screen.
checkBouncing :: Float -> Float -> Ball -> Ball
checkBouncing width height b@(Ball i pos@(x,y) (xspeed,yspeed) accel r)
  | (x > width) = Ball i (2*width-x, y) ((-1) * xspeed,yspeed) accel r
  | (x < 0) = Ball i ((-1)*x,y) ((-1) * xspeed,yspeed) accel r
  | (y > height) = Ball i (x, 2*height-y) (xspeed,(-1) * yspeed) accel r
  | (y < 0) = Ball i (x,(-1)*y) (xspeed, (-1)*yspeed) accel r
  | otherwise = b
