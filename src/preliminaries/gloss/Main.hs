{-|
Module      : Main
Description : Gloss minimal demo.
Copyright   : (c) José Edil G. de Medeiros, 2018
License     : MIT
Maintainer  : jose.edil@gmail.com
Stability   : experimental
Portability : POSIX

This module shows the minimal setup to use Gloss
to display something on the screen.

-}

module Main
  ( main
  ) where

import Graphics.Gloss

-- |'window' defines a Display object to setup the screen.
window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

-- |'background' color.
background :: Color
background = white

--
-- |Define 'strokeColor': dark gray
strokeColor :: Color
strokeColor = greyN 0.3

-- |Define 'fillColor': translucent gray.
fillColor :: Color
fillColor = withAlpha 0.3 black

-- | A 'ball' is a filled circle.
ball :: Picture
ball =
  pictures
    [color fillColor $ circleSolid 15,
     color strokeColor $ thickCircle 15 1.8]

-- | 'main' function.
main :: IO ()
main = display window background ball
