module Tetrafall.Particle.Physics
  ( vec2Zero
  , vec2Add
  , vec2Sub
  , vec2Scale
  , vec2Length
  , vec2LengthSquared
  , vec2Normalize
  , vec2Dot
  , gravity
  , timeStep
  , updateFireworkParticle
  , calculateLifeState
  , initFireworkParticle
  ) where

import Data.Time.Clock (NominalDiffTime)
import Tetrafall.Types

-- Vector operations

vec2Zero :: Vec2
vec2Zero = Vec2 0.0 0.0

vec2Add :: Vec2 -> Vec2 -> Vec2
vec2Add (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

vec2Sub :: Vec2 -> Vec2 -> Vec2
vec2Sub (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

vec2Scale :: Float -> Vec2 -> Vec2
vec2Scale s (Vec2 x y) = Vec2 (s * x) (s * y)

vec2LengthSquared :: Vec2 -> Float
vec2LengthSquared (Vec2 x y) = x * x + y * y

vec2Length :: Vec2 -> Float
vec2Length v = sqrt (vec2LengthSquared v)

vec2Normalize :: Vec2 -> Vec2
vec2Normalize v =
  let len = vec2Length v
  in if len > 0.0001
     then vec2Scale (1.0 / len) v
     else vec2Zero

vec2Dot :: Vec2 -> Vec2 -> Float
vec2Dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

-- Physics constants

gravity :: Vec2
gravity = Vec2 0.0 10.0

timeStep :: Float
timeStep = 0.001

-- Particle lifecycle

calculateLifeState :: NominalDiffTime -> NominalDiffTime -> ParticleLifeState
calculateLifeState elapsed lifetime
  | ratio >= (1.0 :: Float) = ParticleDead
  | ratio >= (0.9 :: Float) = ParticleDying
  | ratio >= (0.7 :: Float) = ParticleDeclining
  | otherwise = ParticleAlive
  where
    ratio = realToFrac elapsed / realToFrac lifetime :: Float

-- Initialize a firework particle from config

initFireworkParticle :: FireworkParticleConfig -> FireworkParticle
initFireworkParticle config = FireworkParticle
  { _fpPos = _fpcInitPos config
  , _fpVel = _fpcInitVel config
  , _fpTrail = []
  , _fpLifeState = ParticleAlive
  , _fpTimeElapsed = 0
  , _fpConfig = config
  }

-- Physics simulation

updateFireworkParticle :: NominalDiffTime -> FireworkConfig -> FireworkParticle -> FireworkParticle
updateFireworkParticle dt config particle =
  let newElapsed = _fpTimeElapsed particle + dt
      lifetime = _fpcLifeTime (_fpConfig particle)
      newLifeState = calculateLifeState newElapsed lifetime
      
      -- Only update physics if particle is not dead
      (newPos, newVel, newTrail) = if newLifeState /= ParticleDead
                                    then updatePhysics dt config particle
                                    else (_fpPos particle, _fpVel particle, _fpTrail particle)
  in particle
     { _fpPos = newPos
     , _fpVel = newVel
     , _fpTrail = newTrail
     , _fpLifeState = newLifeState
     , _fpTimeElapsed = newElapsed
     }

updatePhysics :: NominalDiffTime -> FireworkConfig -> FireworkParticle -> (Vec2, Vec2, [Vec2])
updatePhysics dt config particle =
  let dt' = realToFrac dt
      numSteps = ceiling (dt' / timeStep)
      stepDt = dt' / fromIntegral numSteps
      
      currentPos = _fpPos particle
      currentVel = _fpVel particle
      currentTrail = _fpTrail particle
      
      (finalPos, finalVel, finalTrail) = iteratePhysics numSteps stepDt config particle currentPos currentVel currentTrail
  in (finalPos, finalVel, finalTrail)

iteratePhysics :: Int -> Float -> FireworkConfig -> FireworkParticle -> Vec2 -> Vec2 -> [Vec2] -> (Vec2, Vec2, [Vec2])
iteratePhysics 0 _ _ _ pos vel trail = (pos, vel, trail)
iteratePhysics n dt config particle pos vel trail =
  let -- Calculate forces
      gravityForce = vec2Scale (_fcGravityScale config) gravity
      
      -- Air resistance: -v * |v|^2 * ar_scale
      velLength = vec2Length vel
      velNormalized = vec2Normalize vel
      airResistance = vec2Scale (negate (velLength * velLength * _fcAirResistanceScale config)) velNormalized
      
      -- Additional custom forces
      additionalForce = _fcAdditionalForce config particle
      
      -- Total acceleration
      totalAccel = gravityForce `vec2Add` airResistance `vec2Add` additionalForce
      
      -- Update velocity and position (Euler integration)
      newVel = vel `vec2Add` vec2Scale dt totalAccel
      newPos = pos `vec2Add` vec2Scale dt newVel
      
      -- Update trail
      maxTrailLength = _fpcTrailLength (_fpConfig particle)
      newTrail = take maxTrailLength (pos : trail)
  in iteratePhysics (n - 1) dt config particle newPos newVel newTrail
