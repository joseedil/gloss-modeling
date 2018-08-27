module Config where

import Graphics.Gloss

-- World setup
numBalls :: Int
numBalls = 500

-- Display settings
-- | Display width.
dispWidth :: Int
dispWidth = 640

-- | Display height.
dispHeight :: Int
dispHeight = 360

-- Timestep = 300 steps/sec
simResolution :: Int
simResolution = 300

-- Gravity coefficient
gravityCoeff :: Float
gravityCoeff = 300

-- Render velocity vector?
showBeadVelocity :: Bool
showBeadVelocity = False

-- Colors
beadStrokeColor :: Color
beadStrokeColor = greyN 0.3

beadFillColor :: Color
beadFillColor = withAlpha 0.3 black

velVectorColor :: Color
velVectorColor = red

accelVectorColor :: Color
accelVectorColor = blue
