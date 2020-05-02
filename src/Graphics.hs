module Graphics
    where

import GameObjects
import Data.Text
import qualified SDL

windowHeight :: Int
windowHeight = 500

windowWidth :: Int
windowWidth = 600

groundHeight :: Int
groundHeight = windowHeight `div` 8

class Renderable a where
    render :: SDL.Renderer -> a -> IO ()

instance Renderable Player where
    render renderer (Player position _) = drawSquare renderer position 50

instance Renderable Obstacle where
    render renderer (Obstacle height position) = drawSquare renderer position height

instance Renderable Game where
    render renderer (InProgress player score obstacles) = do
        render renderer player 
        mapM_ (render renderer) obstacles
    render renderer (Finished score) = undefined

drawRectangle :: SDL.Renderer -> Position -> (Int, Int) -> IO ()
drawRectangle renderer (Position px py) (x, y) = setRenderAttrs >> renderShape
    where 
        setRenderAttrs = SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 maxBound
        renderShape = SDL.fillRect renderer $ Just $
                                     SDL.Rectangle (SDL.P (SDL.V2 (toEnum px) (toEnum py)))
                                                   (SDL.V2 (toEnum x) (toEnum y))

drawSquare :: SDL.Renderer -> Position -> Int -> IO ()
drawSquare renderer (Position x y) size = do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 maxBound
    SDL.fillRect renderer $ Just $
                                     SDL.Rectangle (SDL.P (SDL.V2 (toEnum x) (toEnum $ windowHeight - y - size)))
                                                   (SDL.V2 (toEnum size) (toEnum size))