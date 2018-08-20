module Main
  ( main
  ) where

import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

--
strokeColor :: Color
strokeColor = greyN 0.3

fillColor :: Color
fillColor = withAlpha 0.3 black

ball :: Picture
ball =
  pictures
    [color fillColor $ circleSolid 15,
     color strokeColor $ thickCircle 15 1.8]

--
drawing = pictures [ball,
                    translate 12 0 ball,
                    translate 6 (-12) ball]

main :: IO ()
main = display window background drawing
