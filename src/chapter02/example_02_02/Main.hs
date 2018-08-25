-- Bouncing ball example

module Main
  ( main
  ) where

import Config
import Ball

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import System.Random

-- RNG and initial setup --
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let
    (value, newGen) = random gen
    (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in
    (value:restOfList, finalGen)

initialMass :: [Float]
initialMass = fst . finiteRandoms (numBalls) $ mkStdGen 0

initialX :: [Float]
initialX = map (\x -> x*(fromIntegral dispWidth)) $ fst . finiteRandoms (numBalls) $ mkStdGen 10

initialY :: [Float]
initialY = map (\x -> x*(fromIntegral dispHeight)) $ fst . finiteRandoms (numBalls) $ mkStdGen 20


-- | Ball models
balls :: [Ball]
balls =
  let
    createBall i pos m = Ball i pos nullVector nullVector m
  in
    map (\i -> createBall i (initialX!!i, initialY!!i) (10 + 10 * (initialMass !! i))) [0..numBalls-1]

-- | Gravity
gravity :: Vector
gravity = (0,-0.1)

-- | Wind
wind :: Vector
wind = (0.01,0)

-- | Move the ball.
moveBall :: Ball -> Ball
moveBall = (checkBouncing dispWidth dispHeight) . updateBall . (applyForce gravity) . (applyForce wind)

moveBalls :: ViewPort -> Float -> [Ball] -> [Ball]
moveBalls _ _ = map moveBall

renderBalls = pictures . map renderBall

-- | Translate Viewport origin to the left-bottom extreme of the window.
renderWorld :: [Ball] -> Picture
renderWorld =
  let
    view = ViewPort
           ((-1)*(fromIntegral $ dispWidth)/2
           ,(-1)*(fromIntegral $ dispHeight)/2)
           0
           1
  in
    applyViewPortToPicture view . renderBalls

-- | Simulation
main :: IO ()
main =
  do
    simulate
      (InWindow
       "Nice Window"               -- Window title
       (dispWidth,dispHeight)      -- Window size
       (0, 0))                     -- Window position
      white                         -- Window background
      simResolution                 -- Time resolution
      balls                          -- Initial model
      renderWorld                   -- Model render function
      moveBalls                      -- Model update function
