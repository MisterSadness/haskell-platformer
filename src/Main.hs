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

main :: IO ()
main = animate (pack "Game") windowWidth windowHeight (parseWinInput >>> (game &&& handleExit))

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

    lastDoubleeraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastDoubleeraction currentTime
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
    player <- stopPlayer groundHeight -< Player pos v a0
    returnA -< player

stopPlayer :: Double -> SF Player Player
stopPlayer bound = arr stop 
    where
        stop p@(Player (x, y) (vx, _) _) = 
            if y <= bound 
                then Player (x, bound) (vx, 0) noAcceleration 
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
speedUp MoveLeft (Player pos (vx, vy) acc) = Player pos (vx-40, vy) acc
speedUp MoveRight (Player pos (vx, vy) acc) = Player pos (vx+40, vy) acc
speedUp Jump (Player (x, y) (vx, vy) _) = Player (x, y) (vx, vy+100) normalAcceleration 

game :: SF AppInput Game
game = switch sf $ const game
    where
        sf = proc input -> do
            gameState <- gameSession -< input
            gameOver <- edge -< False
            returnA -< (gameState, gameOver)

gameSession :: SF AppInput Game
gameSession = proc input -> do
    player <- jumpingPlayer defaultPlayer -< input
    score <- distance -< player
    returnA -< InProgress player score []

distance :: SF Player Distance
distance = arr (\(Player (x, _) _ _) -> x)

