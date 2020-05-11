module GameObjects where

type Position = (Double, Double)
type Velocity = (Double, Double)
type Acceleration = (Double, Double)

type Distance = Double
type Score = Double

data Player = Player
    {
        playerPosition :: Position,
        playerVelocity :: Velocity,
        playerAcceleration :: Acceleration
    }
data Obstacle = Obstacle { obstacleHeight :: Double, obstaclePosition :: Position }

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
gravity = -50

defaultPlayer :: Player
defaultPlayer = Player (10, groundHeight) noVelocity noAcceleration

middlePlayer :: Player
middlePlayer = Player (windowWidth / 2 - 50, windowHeight / 2 - 50)
                      noVelocity
                      noAcceleration

infiniteObstacles :: [Obstacle]
infiniteObstacles = map (\x -> Obstacle 50 (x, 0)) [100, 300 ..]
