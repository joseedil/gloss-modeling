module Ball where

import Graphics.Gloss

data Ball = Ball
            Point          -- Center
            Point          -- Velocity
            Float          -- Radius
            deriving Show

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

move :: Ball -> Ball
move (Ball (x,y) (xspeed,yspeed) r) =
  Ball (x+xspeed, y+yspeed) (xspeed,yspeed) r
