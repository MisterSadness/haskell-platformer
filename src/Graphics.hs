module Graphics
    where

import Data.Text
import qualified SDL

createWindow :: IO SDL.Window
createWindow = SDL.createWindow (pack "Jump!") SDL.defaultWindow