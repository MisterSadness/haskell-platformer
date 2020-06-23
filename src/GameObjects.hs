{-# LANGUAGE TemplateHaskell #-}
-- | This module contains definitons for the custom types used in game logic, some default values and constants.
module GameObjects(
    -- * Game specific types
    Player (..),
    Obstacle (..),
    Game (..),
    Position,
    Velocity,
    Acceleration,
    Distance,
    -- * Predefined values for Player and [Obstacle]
    defaultPlayer,
    infiniteObstacles,
    visibleObstacles,
    -- * Some constants
    windowHeight,
    windowWidth,
    groundHeight,
    playerSize,
    playerOffset,
    -- * Lenses for Player and Obstacle
    playerPosition,
    playerVelocity,
    playerAcceleration,
    obstacleHeight,
    obstaclePosition
) where

import           Control.Lens

-- | A tuple of containing a position (x,y)
type Position = (Double, Double)
-- | A tuple of velocity (v_x, v_y)
type Velocity = (Double, Double)
-- | A tuple of acceleration (a_x, a_y)
type Acceleration = (Double, Double)
-- | Distance the player travelled, stored as floating point
type Distance = Double

-- | A player is defined by his position, velocity and acceleration
data Player = Player
    {
        _playerPosition :: Position,
        _playerVelocity :: Velocity,
        _playerAcceleration :: Acceleration
    }
makeLenses ''Player

-- | A single obstacle with a given height and position
data Obstacle = Obstacle { _obstacleHeight :: Double, _obstaclePosition :: Position }
makeLenses ''Obstacle

-- | The whole game, consisting of a player, the distance he travelled, and a list of obstacles
data Game = Game Player Distance [Obstacle]

-- | Default player is standing on the ground with no velocity and acceleration
defaultPlayer :: Player
defaultPlayer = Player (30, groundHeight) (0,0) (0,-150)

-- | An infinite list of obstacles of height 50, located at distances (2n-1)*100
infiniteObstacles :: [Obstacle]
infiniteObstacles = map (\x -> Obstacle 50 (x, groundHeight)) [100, 300 ..]

-- | A function that determines the list of obstacles that are currently on the screen, based on player position
visibleObstacles :: Player -> [Obstacle] -> [Obstacle]
visibleObstacles player =
    map (obstaclePosition . _1 -~ left)
        . takeWhile ((right >) . view (obstaclePosition . _1))
        . dropWhile (\(Obstacle size (x,_))-> size+x < left)
  where
    left  = player ^. playerPosition . _1 - playerOffset
    right = left + windowWidth

-- | Height of the window in pixels
windowHeight :: Double
windowHeight = 500

-- | Width of the window in pixels
windowWidth :: Double
windowWidth = 600

-- | Ground height in pixels
groundHeight :: Double
groundHeight = 60

-- | How far the player is from the left border at all times
playerOffset :: Double
playerOffset = 30

-- | How big is the player character
playerSize :: Double
playerSize = 50