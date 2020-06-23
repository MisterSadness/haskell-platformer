-- | This module defines a Renderable class, that contains objects that can be represented on the screen in some way.
module Graphics (
    -- * Renderable instances
    Renderable (..)
) where

import           GameObjects
import qualified SDL
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Colour.Names

class Renderable a where
    render :: SDL.Renderer -> a -> IO ()

instance Renderable Player where
    -- | Draws a square at player's position
    render renderer (Player (_, y) _ _) = drawSquare renderer (playerOffset, y) playerSize

instance Renderable Obstacle where
    -- | Draws a square at obstacle's position, with size equal to obstacle's height
    render renderer (Obstacle height position) =
        drawSquare renderer position height

instance Renderable Game where
    -- | Renders all parts of the game, using the corresponding Renderable definitions for Player and Obstacle
    render renderer (Game player _ obstacles) = do
        setColour renderer skyblue      >> SDL.clear renderer
        setColour renderer green        >> drawRectangle renderer (0, 0) (windowWidth, groundHeight)
        setColour renderer darkmagenta  >> render renderer player
        setColour renderer brown        >> mapM_ (render renderer) (visibleObstacles player obstacles)

-- helper functions, not exported
drawRectangle :: SDL.Renderer -> Position -> (Double, Double) -> IO ()
drawRectangle renderer (px, py) (width, height) =
    SDL.fillRect renderer $ Just $ SDL.Rectangle
        (SDL.P (SDL.V2 (convert px) (convert (windowHeight - py - height))))
        (SDL.V2 (convert width) (convert height))
    where convert = toEnum . round

drawSquare :: SDL.Renderer -> Position -> Double -> IO ()
drawSquare renderer pos size = drawRectangle renderer pos (size, size)

setColour :: SDL.Renderer -> Colour Double -> IO ()
setColour renderer colour =
    let (RGB r g b) = toSRGB24 colour
    in  SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b maxBound
