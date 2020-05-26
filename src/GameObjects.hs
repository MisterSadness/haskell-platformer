{-# LANGUAGE TemplateHaskell #-}
module GameObjects where

import           Control.Lens

type Position = (Double, Double)
type Velocity = (Double, Double)
type Acceleration = (Double, Double)

type Distance = Double
type Score = Double

data Player = Player
    {
        _playerPosition :: Position,
        _playerVelocity :: Velocity,
        _playerAcceleration :: Acceleration
    }
makeLenses ''Player

data Obstacle = Obstacle { _obstacleHeight :: Double, _obstaclePosition :: Position }
makeLenses ''Obstacle

data Game = InProgress Player Distance [Obstacle] | Finished Score

windowHeight :: Double
windowHeight = 500

windowWidth :: Double
windowWidth = 600

groundHeight :: Double
groundHeight = 60

noVelocity :: Velocity
noVelocity = (0, 0)

noAcceleration :: Acceleration
noAcceleration = (0, 0)

normalAcceleration :: Acceleration
normalAcceleration = (0, gravity)

gravity :: Double
gravity = -150

playerOffset :: Double
playerOffset = 30

playerSize :: Double
playerSize = 50

defaultPlayer :: Player
defaultPlayer = Player (playerOffset, groundHeight) noVelocity normalAcceleration

infiniteObstacles :: [Obstacle]
infiniteObstacles = map (\x -> Obstacle 50 (x, groundHeight)) [100, 300 ..]

visibleObstacles :: Player -> [Obstacle] -> [Obstacle]
visibleObstacles player =
    map (obstaclePosition . _1 -~ left)
        . takeWhile ((right >) . view (obstaclePosition . _1))
        . dropWhile (\(Obstacle size (x,_))-> size+x < left)
  where
    left  = player ^. playerPosition . _1 - playerOffset
    right = left + windowWidth
