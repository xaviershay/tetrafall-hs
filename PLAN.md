# Firework Particle System Integration Plan

## Progress Tracker
- [x] Phase 1: Core Data Types ✅ Complete
- [ ] Phase 2: Physics Module
- [ ] Phase 3: Firework Generation
- [ ] Phase 4: Game Integration
- [ ] Phase 5: Rendering
- [ ] Testing

## Overview
Port the firework particle system from [firework-rs](https://github.com/Wayoung7/firework-rs) to the Tetrafall Haskell game to create a new `ParticleFirework` type with realistic physics-based animation.

## Current State Analysis

### Existing Particle System
- **Location**: `src/Tetrafall/Types.hs`
- **Current Implementation**:
  - Single particle type: `ParticleStar`
  - Basic particle structure with location and age
  - Particles spawned randomly each step
  - No physics simulation
  - Stored in `Game` state as `_particles :: [Particle]`

### Firework-rs Algorithm Analysis

The Rust implementation has a sophisticated particle system with:

1. **Core Data Structures**:
   - `Particle`: Individual particle with position, velocity, trail, lifetime, color
   - `ParticleConfig`: Template for creating particles (init_pos, init_vel, trail_length, life_time, color)
   - `Firework`: Collection of particles spawned together
   - `FireworkConfig`: Physics parameters (gravity_scale, ar_scale, additional_force, gradient_scale)
   - `FireworkManager`: Manages multiple fireworks

2. **Physics Simulation** (`particle.rs` update function):
   ```rust
   const TIME_STEP: f32 = 0.001;
   self.vel += TIME_STEP * (
       Vec2::Y * 10. * config.gravity_scale  // Gravity
       - self.vel.normalize() * self.vel.length().powi(2) * config.ar_scale  // Air resistance
       + (config.additional_force)(self)  // Custom forces
   );
   self.pos += TIME_STEP * self.vel;
   ```
   - Uses Euler integration with small time steps
   - Gravity: 10 units/s² (downward)
   - Air resistance: quadratic drag force
   - Additional forces: e.g., vortex attraction

3. **Particle Lifecycle**:
   - States: `Alive` → `Declining` → `Dying` → `Dead`
   - Based on `time_elapsed / life_time` ratio
   - Trail: VecDeque of previous positions (for visual effect)

4. **Explosion Forms**:
   - `Instant`: All particles spawn at once
   - `Sustained`: Particles spawn over time at intervals (fountain effect)

5. **Visual Effects**:
   - Color gradient based on lifetime progress
   - Trail rendering with density falloff
   - Different characters for different life states

## Implementation Plan

### Phase 1: Core Data Types (src/Tetrafall/Types.hs)

#### 1.1 Particle Physics Types
```haskell
data Vec2 = Vec2
  { _vec2X :: Float
  , _vec2Y :: Float
  } deriving (Eq, Show)

data ParticleLifeState = 
    ParticleAlive 
  | ParticleDeclining 
  | ParticleDying 
  | ParticleDead
  deriving (Eq, Show)
```

#### 1.2 Firework Particle Configuration
```haskell
data FireworkParticleConfig = FireworkParticleConfig
  { _fpcInitPos :: Vec2
  , _fpcInitVel :: Vec2
  , _fpcTrailLength :: Int
  , _fpcLifeTime :: NominalDiffTime
  , _fpcColor :: (Word8, Word8, Word8)  -- RGB
  } deriving (Eq, Show)
```

#### 1.3 Firework Physics Configuration
```haskell
data FireworkConfig = FireworkConfig
  { _fcGravityScale :: Float        -- Multiplier for gravity (default: 1.0)
  , _fcAirResistanceScale :: Float  -- Air drag coefficient (default: 0.28)
  , _fcAdditionalForce :: FireworkParticle -> Vec2  -- Custom force function
  , _fcGradientScale :: Float -> Float  -- Color gradient function (0-1 input)
  , _fcEnableGradient :: Bool
  }
```

#### 1.4 Firework Particle
```haskell
data FireworkParticle = FireworkParticle
  { _fpPos :: Vec2
  , _fpVel :: Vec2
  , _fpTrail :: [Vec2]  -- Most recent at head
  , _fpLifeState :: ParticleLifeState
  , _fpTimeElapsed :: NominalDiffTime
  , _fpConfig :: FireworkParticleConfig
  } deriving (Eq, Show)
```

#### 1.5 Update ParticleType
```haskell
data ParticleType = 
    ParticleStar 
  | ParticleFirework FireworkParticle
  deriving (Eq, Show)

data Particle = Particle
  { _particleLocation :: (Float, Float)  -- Keep for compatibility
  , _particleAge :: Int
  , _particleType :: ParticleType
  } deriving (Eq, Show)
```

### Phase 2: Physics Module (src/Tetrafall/Particle/Physics.hs)

Create new module for firework physics simulation:

```haskell
module Tetrafall.Particle.Physics where

-- Vector operations
vec2Zero :: Vec2
vec2Add :: Vec2 -> Vec2 -> Vec2
vec2Scale :: Float -> Vec2 -> Vec2
vec2Length :: Vec2 -> Float
vec2Normalize :: Vec2 -> Vec2

-- Physics constants
gravity :: Vec2  -- (0, 10)
timeStep :: Float  -- 0.001

-- Physics simulation
updateFireworkParticle :: NominalDiffTime -> FireworkConfig -> FireworkParticle -> FireworkParticle
calculateLifeState :: NominalDiffTime -> NominalDiffTime -> ParticleLifeState
```

**Physics Implementation**:
- Use Euler integration with 0.001s time step (same as Rust version)
- Gravity: 10 units/s² downward
- Air resistance: quadratic drag `v² * ar_scale`
- Support additional force functions (for vortex, attraction effects)

### Phase 3: Firework Generation (src/Tetrafall/Particle/Firework.hs)

Module for creating firework patterns:

```haskell
module Tetrafall.Particle.Firework where

-- Velocity generation patterns (port from utils.rs)
genPointsCircle :: Float -> Int -> StdGen -> ([Vec2], StdGen)
genPointsCircleNormal :: Float -> Int -> StdGen -> ([Vec2], StdGen)
genPointsFan :: Float -> Int -> Float -> Float -> StdGen -> ([Vec2], StdGen)

-- Gradient functions (port from utils.rs)
explosionGradient1 :: Float -> Float
explosionGradient2 :: Float -> Float
linearGradient1 :: Float -> Float

-- Firework presets
createBasicFirework :: Vec2 -> [(Word8, Word8, Word8)] -> StdGen -> ([FireworkParticle], StdGen)
createRocketFirework :: Vec2 -> StdGen -> ([FireworkParticle], StdGen)
createFountainFirework :: Vec2 -> StdGen -> ([FireworkParticle], StdGen)
```

### Phase 4: Integration with Game State

#### 4.1 Game Trigger
When lines are cleared, spawn firework at cleared line positions:

```haskell
-- In Game.hs lockPiece function
lockPiece :: Tetromino -> Game -> Game
lockPiece piece game =
  let currentPieceGrid = getTetrominoGrid piece
      baseGrid = game ^. grid
      gridWithPiece = baseGrid `overlay` currentPieceGrid
      (newGrid, linesCleared, clearedYPositions) = clearLinesWithPositions gridWithPiece
      scorePoints = calculateScore game linesCleared
      newFireworks = if linesCleared > 0
                     then spawnFireworksForLines clearedYPositions game
                     else []
  in game & grid .~ newGrid 
          & currentPiece .~ Nothing
          & slideState .~ CanFall
          & score %~ (+ scorePoints)
          & particles %~ (++ newFireworks)
```

#### 4.2 Particle Update
Add firework particle updates to game step:

```haskell
-- In Game.hs
applyTick :: NominalDiffTime -> Game -> Game
applyTick dt game =
  let newAccum = game ^. gameStepAccum + dt
      stepSize = game ^. gameStepSize
      gameWithTime = game & gameTime %~ (+ dt)
      -- Update all particles
      updatedParticles = updateParticles dt (game ^. particles)
      gameWithParticles = gameWithTime & particles .~ updatedParticles
  in if newAccum >= stepSize
     then step (gameWithParticles & gameStepAccum .~ (newAccum - stepSize))
     else gameWithParticles & gameStepAccum .~ newAccum

updateParticles :: NominalDiffTime -> [Particle] -> [Particle]
updateParticles dt = filter (not . isParticleDead) . map (updateParticle dt)

updateParticle :: NominalDiffTime -> Particle -> Particle
updateParticle dt particle = case _particleType particle of
  ParticleStar -> particle & particleAge %~ (+1)  -- Existing behavior
  ParticleFirework fp -> 
    let config = defaultFireworkConfig  -- Or get from game state
        updatedFp = updateFireworkParticle dt config fp
    in particle & particleType .~ ParticleFirework updatedFp
```

### Phase 5: Rendering (Will require Main.hs changes)

The rendering will need to:
1. Convert particle world coordinates to terminal coordinates
2. Render trails with appropriate characters based on life state
3. Apply color gradients if enabled
4. Handle coordinate system differences (game grid vs terminal)

**Note**: The actual rendering implementation depends on the terminal rendering library being used (likely brick or ansi-terminal). This will be implemented in the main application code.

## Configuration and Defaults

### Default Firework Configurations

Based on demo_firework_0 from the Rust code:

```haskell
defaultFireworkConfig :: FireworkConfig
defaultFireworkConfig = FireworkConfig
  { _fcGravityScale = 1.0
  , _fcAirResistanceScale = 0.28
  , _fcAdditionalForce = \_ -> vec2Zero
  , _fcGradientScale = explosionGradient1
  , _fcEnableGradient = True
  }

-- Line clear firework: explosion pattern with random colors
lineClearFirework :: Vec2 -> StdGen -> ([FireworkParticle], StdGen)
lineClearFirework center rng = 
  let colors = [(255, 102, 75), (144, 56, 67), (255, 225, 124), (206, 32, 41)]
      numParticles = 45
      velocity = 250.0  -- Radius of explosion
      (velocities, rng') = genPointsCircleNormal velocity numParticles rng
      particles = zipWith (createParticle center colors) velocities [0..]
  in (particles, rng')
  where
    createParticle center colors vel idx =
      let (color, _) = randomChoice colors (mkStdGen idx)
          config = FireworkParticleConfig
            { _fpcInitPos = center
            , _fpcInitVel = vel
            , _fpcTrailLength = randomRange 23 27 (mkStdGen idx)
            , _fpcLifeTime = randomRangeFloat 2.1 2.7 (mkStdGen (idx + 1000))
            , _fpcColor = color
            }
      in initFireworkParticle config
```

## Testing Strategy

### Unit Tests (test/Particle/Firework.hs)

1. **Physics Tests**:
   - Verify gravity pulls particles downward
   - Verify air resistance slows particles
   - Verify particles follow expected trajectories
   
2. **Lifecycle Tests**:
   - Particles transition through life states correctly
   - Particles marked dead after lifetime expires
   - Trail length maintained correctly

3. **Generation Tests**:
   - Velocity generation produces expected patterns
   - Random particle generation is consistent
   - Different firework types create correct particle counts

### Integration Tests (test/Game.hs)

1. **Game Integration**:
   - Fireworks spawn when lines cleared
   - Particles update each tick
   - Dead particles removed from game state
   - Game performance remains acceptable

## Implementation Order

1. **Phase 1**: Core data types and Vec2 operations (1-2 hours)
2. **Phase 2**: Physics simulation module (2-3 hours)
3. **Phase 3**: Firework generation patterns (2-3 hours)
4. **Phase 4**: Game state integration (1-2 hours)
5. **Phase 5**: Rendering (3-4 hours, depends on existing renderer)
6. **Testing**: Unit and integration tests (2-3 hours)

**Total Estimate**: 12-17 hours of development time

## Future Enhancements

After basic implementation, consider:

1. **Multiple Firework Types**:
   - Sustained fireworks (fountains)
   - Rocket fireworks (ascent then explosion)
   - Vortex effects (particles spiral)
   - Heart shapes or other patterns

2. **Trigger Variations**:
   - Different fireworks for different combo sizes
   - Special fireworks for tetris (4 lines at once)
   - Fireworks on game level up

3. **Performance Optimization**:
   - Particle pooling
   - Spatial partitioning if many particles
   - Early culling of off-screen particles

4. **Visual Effects**:
   - Color palettes matching tetromino colors
   - Gradient effects
   - Particle glow/bloom effects

## Dependencies

### New Dependencies to Add
```yaml
dependencies:
  - vector-space  # For Vec2 operations (or implement manually)
```

### Existing Dependencies Used
- `microlens-platform` (already in use for lenses)
- `random` (already in use for RNG)
- `time` (already in use for NominalDiffTime)

## Notes and Considerations

1. **Coordinate Systems**: 
   - Game grid uses integer coordinates (0-based, top-left origin)
   - Firework particles use float coordinates for smooth physics
   - Need conversion when spawning and rendering

2. **Performance**:
   - Each firework spawns 25-150 particles
   - Each particle updates every tick (potentially 60fps)
   - Trail storage adds memory overhead
   - Consider limiting max active particles (e.g., 500)

3. **Physics Accuracy**:
   - The Rust version uses 0.001s time steps for accuracy
   - May need to adjust based on game tick rate
   - Higher tick rates may need fewer sub-steps

4. **Randomness**:
   - Need to thread StdGen through particle generation
   - Consider storing separate RNG in Game for particles
   - Ensure consistent behavior across different systems

5. **Compatibility**:
   - Keep existing `ParticleStar` working during transition
   - Gradually migrate to new system
   - Ensure tests pass at each phase

## References

- Original Rust implementation: https://github.com/Wayoung7/firework-rs
- Key files to reference:
  - `src/particle.rs`: Core particle physics
  - `src/fireworks.rs`: Firework management
  - `src/demo.rs`: Example firework configurations
  - `src/utils.rs`: Velocity generation patterns
