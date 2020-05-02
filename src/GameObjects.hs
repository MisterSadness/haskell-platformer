module GameObjects
    where

data Position = Position { xPosition :: Int, yPosition :: Int }
data Velocity = Velocity { xVelocity :: Int, yVelocity :: Int }

type Distance = Int
type Score = Int

data Player = Player { playerPosition :: Position, playerVelocity :: Velocity}
data Obstacle = Obstacle { obstacleHeight :: Int, obstaclePosition :: Position }

data Game = InProgress Player Distance [Obstacle] | Finished Score

defaultPlayer :: Player
defaultPlayer = Player (Position 0 0) (Velocity 0 0)