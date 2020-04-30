module GameObjects
    where

data Player = Player { playerHeight :: Double, playerVelocity :: Double}

type Score = Int

data Obstacle = Obstacle { obstacleHeight :: Double, obstaclePosition :: Double }

data Game = InProgress Player Score Obstacle | Finished Score
