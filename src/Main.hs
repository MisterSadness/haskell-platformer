{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import qualified SDL
import Data.Text
import Control.Concurrent.MVar
import Control.Monad
import Graphics
import GameObjects
import Input
import Control.Lens

main :: IO ()
main = animate (pack "Game") windowWidth windowHeight (parseWinInput >>> (gameSession &&& handleExit))

animate :: Renderable a
        => Text
        -> Double
        -> Double
        -> SF (FRP.Yampa.Event SDL.EventPayload) (a, Bool)
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    lastDoublereaction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastDoublereaction currentTime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (game_, shouldExit) = do
            when changed $ do
                render renderer game_
                SDL.present renderer
            return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where windowConf = SDL.defaultWindow
            { SDL.windowInitialSize = SDL.V2 (round winWidth) (round winHeight) }

movingPlayer :: Player -> SF a Player
movingPlayer (Player pos0 v0 a0) = proc _ -> do
    v <- imIntegral v0 -< a0
    pos <- imIntegral pos0 -< v
    returnA -< stop groundHeight $ Player pos v a0
    where
        stop bound p@(Player (x, y) (vx, _) a) =
            if y <= bound
                then Player (x, bound) (vx, 0) a
                else p

jumpingPlayer :: Player -> SF AppInput Player
jumpingPlayer player0 = switch sf cont
    where
        sf = proc input -> do
            player <- movingPlayer player0 -< ()
            command <- parseAppInput -< input
            returnA -< (player, command `attach` player)
        cont (command, player) = jumpingPlayer $ speedUp command player

speedUp :: Command -> Player -> Player
speedUp MoveLeft = (playerVelocity._1) -~ 40
speedUp MoveRight = (playerVelocity._1) +~ 40
speedUp Jump = (playerVelocity._2) +~ 200

gameSession :: SF AppInput Game
gameSession = proc input -> do
    player <- jumpingPlayer defaultPlayer -< input
    score <- distance -< player
    obstacles <- identity -< infiniteObstacles
    returnA -< InProgress player score obstacles

distance :: SF Player Distance
distance = arr (\(Player (x, _) _ _) -> x-playerOffset)
