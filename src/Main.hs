{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import FRP.Yampa.Switches
import FRP.Yampa.Arrow
import qualified SDL

import Data.Text
import Control.Concurrent.MVar
import Control.Monad

import Graphics
import GameObjects
import Input

main :: IO ()
main = animate (pack "Game") windowWidth windowHeight 
        (parseWinInput >>> (game &&& handleExit))

-- initialState = undefined
-- senseInput = undefined
-- consumeOutput = undefined
-- mainSf = undefined

animate :: Text                -- ^ window title
        -> Int                 -- ^ window width in pixels
        -> Int                 -- ^ window height in pixels
        -> SF (FRP.Yampa.Event SDL.EventPayload) (Game, Bool) -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 maxBound maxBound maxBound maxBound

    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (game_, shouldExit) = do
            when changed $ do
                SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
                SDL.clear renderer
                render renderer game_
                SDL.present renderer
            return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where 
        windowConf = SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral winWidth) (fromIntegral winHeight) }


game :: SF AppInput Game
game = switch sf (\_ -> game)
    where sf = proc input -> do
              gameState <- gameSession -< input
              gameOver <- edge -< False 
              returnA -< (gameState, gameOver)

gameSession :: SF AppInput Game
gameSession = proc input -> do
    player <- movingPlayer defaultPlayer -< input
    returnA -< InProgress player 0 []

movingPlayer :: Player -> SF AppInput Player
movingPlayer player = dSwitch sf cont
    where
        sf = proc input -> do
            command <- parseAppInput -< input
            returnA -< (player, command)
        cont command = movingPlayer $ move command player
    
move :: Command -> Player -> Player
move MoveLeft (Player (Position x y) vel) = (Player (Position (x-10) y) vel)
move MoveRight (Player (Position x y) vel) = (Player (Position (x+10) y) vel)

defaultGame :: Game
defaultGame = InProgress (Player (Position 10 0) (Velocity 0 0)) 0 [Obstacle 75 (Position 100 50)]
