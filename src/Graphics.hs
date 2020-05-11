module Graphics where

import           GameObjects
import qualified SDL
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Colour.Names

class Renderable a where
    render :: SDL.Renderer -> a -> IO ()

instance Renderable Player where
    render renderer (Player position _ _) = drawSquare renderer position 50

instance Renderable Obstacle where
    render renderer (Obstacle height position) =
        drawSquare renderer position height

instance Renderable Game where
    render renderer (InProgress player _ obstacles) = do
        setColour renderer skyblue >> SDL.clear renderer
        setColour renderer green
            >> drawRectangle renderer (0, 0) (windowWidth, groundHeight)
        setColour renderer darkmagenta >> render renderer player
        mapM_ (render renderer) obstacles
    render _ (Finished _) = undefined

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
