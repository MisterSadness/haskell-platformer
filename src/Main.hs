module Main where

import SDL
import Graphics

main :: IO ()
main = do
    initializeAll
    window <- Graphics.createWindow 

    --SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
