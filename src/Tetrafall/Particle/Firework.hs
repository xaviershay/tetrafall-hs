module Tetrafall.Particle.Firework
  ( genPointsCircle
  , genPointsCircleNormal
  , genPointsFan
  , explosionGradient1
  , explosionGradient2
  , linearGradient1
  , defaultFireworkConfig
  , createBasicFirework
  , createLineClearFirework
  , randomChoice
  , randomRange
  , randomRangeFloat
  ) where

import Data.Word (Word8)
import System.Random (StdGen, Random, randomR)
import Tetrafall.Types
import Tetrafall.Particle.Physics

-- Random utility functions

randomChoice :: [a] -> StdGen -> (a, StdGen)
randomChoice [] _ = error "randomChoice: empty list"
randomChoice xs rng =
  let (idx, rng') = randomR (0, length xs - 1) rng
  in (xs !! idx, rng')

randomRange :: Random a => a -> a -> StdGen -> (a, StdGen)
randomRange minVal maxVal rng = randomR (minVal, maxVal) rng

randomRangeFloat :: Float -> Float -> StdGen -> (Float, StdGen)
randomRangeFloat = randomRange

-- Velocity generation patterns

genPointsCircle :: Float -> Int -> StdGen -> ([Vec2], StdGen)
genPointsCircle radius count rng = genPointsCircleImpl radius count 0 rng []
  where
    genPointsCircleImpl :: Float -> Int -> Int -> StdGen -> [Vec2] -> ([Vec2], StdGen)
    genPointsCircleImpl _ total current rng' acc
      | current >= total = (reverse acc, rng')
      | otherwise =
          let angle = 2.0 * pi * fromIntegral current / fromIntegral total
              x = radius * cos angle
              y = radius * sin angle
              point = Vec2 x y
          in genPointsCircleImpl radius total (current + 1) rng' (point : acc)

genPointsCircleNormal :: Float -> Int -> StdGen -> ([Vec2], StdGen)
genPointsCircleNormal baseRadius count rng = genPointsCircleNormalImpl baseRadius count 0 rng []
  where
    genPointsCircleNormalImpl :: Float -> Int -> Int -> StdGen -> [Vec2] -> ([Vec2], StdGen)
    genPointsCircleNormalImpl _ total current rng' acc
      | current >= total = (reverse acc, rng')
      | otherwise =
          let angle = 2.0 * pi * fromIntegral current / fromIntegral total
              (radiusVariation, rng'') = randomRangeFloat 0.8 1.2 rng'
              actualRadius = baseRadius * radiusVariation
              x = actualRadius * cos angle
              y = actualRadius * sin angle
              point = Vec2 x y
          in genPointsCircleNormalImpl baseRadius total (current + 1) rng'' (point : acc)

genPointsFan :: Float -> Int -> Float -> Float -> StdGen -> ([Vec2], StdGen)
genPointsFan radius count startAngle endAngle rng = genPointsFanImpl radius count startAngle endAngle 0 rng []
  where
    genPointsFanImpl :: Float -> Int -> Float -> Float -> Int -> StdGen -> [Vec2] -> ([Vec2], StdGen)
    genPointsFanImpl _ total _ _ current rng' acc
      | current >= total = (reverse acc, rng')
      | otherwise =
          let angleRange = endAngle - startAngle
              angle = startAngle + angleRange * fromIntegral current / fromIntegral (total - 1)
              x = radius * cos angle
              y = radius * sin angle
              point = Vec2 x y
          in genPointsFanImpl radius total startAngle endAngle (current + 1) rng' (point : acc)

-- Gradient functions (map 0.0-1.0 lifetime progress to intensity)

explosionGradient1 :: Float -> Float
explosionGradient1 t
  | t < 0.5 = 1.0
  | otherwise = 2.0 * (1.0 - t)

explosionGradient2 :: Float -> Float
explosionGradient2 t = 1.0 - t

linearGradient1 :: Float -> Float
linearGradient1 t = 1.0 - t * 0.5

-- Default firework configuration

defaultFireworkConfig :: FireworkConfig
defaultFireworkConfig = FireworkConfig
  { _fcGravityScale = 1.0
  , _fcAirResistanceScale = 0.28
  , _fcAdditionalForce = \_ -> vec2Zero
  , _fcGradientScale = explosionGradient1
  , _fcEnableGradient = True
  }

-- Firework creation functions

createBasicFirework :: Vec2 -> [(Word8, Word8, Word8)] -> Float -> Int -> StdGen -> ([FireworkParticle], StdGen)
createBasicFirework center colors velocity numParticles rng =
  let (velocities, rng') = genPointsCircle velocity numParticles rng
  in createParticlesFromVelocities center colors velocities 23 27 2.1 2.7 rng'

createLineClearFirework :: Vec2 -> StdGen -> ([FireworkParticle], StdGen)
createLineClearFirework center rng =
  let colors = [(255, 102, 75), (144, 56, 67), (255, 225, 124), (206, 32, 41)]
      numParticles = 45
      velocity = 250.0
      (velocities, rng') = genPointsCircleNormal velocity numParticles rng
  in createParticlesFromVelocities center colors velocities 23 27 2.1 2.7 rng'

-- Helper function to create particles from velocities

createParticlesFromVelocities :: Vec2 -> [(Word8, Word8, Word8)] -> [Vec2] -> Int -> Int -> Float -> Float -> StdGen -> ([FireworkParticle], StdGen)
createParticlesFromVelocities center colors velocities minTrail maxTrail minLife maxLife rng =
  createParticlesImpl center colors velocities minTrail maxTrail minLife maxLife 0 rng []
  where
    createParticlesImpl :: Vec2 -> [(Word8, Word8, Word8)] -> [Vec2] -> Int -> Int -> Float -> Float -> Int -> StdGen -> [FireworkParticle] -> ([FireworkParticle], StdGen)
    createParticlesImpl _ _ [] _ _ _ _ _ rng' acc = (reverse acc, rng')
    createParticlesImpl pos cols (vel:vels) minT maxT minL maxL idx rng' acc =
      let (color, rng1) = randomChoice cols rng'
          (trailLen, rng2) = randomRange minT maxT rng1
          (lifeTime, rng3) = randomRangeFloat minL maxL rng2
          config = FireworkParticleConfig
            { _fpcInitPos = pos
            , _fpcInitVel = vel
            , _fpcTrailLength = trailLen
            , _fpcLifeTime = realToFrac lifeTime
            , _fpcColor = color
            }
          particle = initFireworkParticle config
      in createParticlesImpl pos cols vels minT maxT minL maxL (idx + 1) rng3 (particle : acc)
