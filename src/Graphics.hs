module Graphics where

import           GameObjects
import qualified SDL
import qualified SDL.Font
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Colour.Names
import           Data.Text

class Renderable a where
    render :: SDL.Renderer -> a -> IO ()

instance Renderable Player where
    render renderer (Player (_, y) _ _) = drawSquare renderer (playerOffset, y) playerSize

instance Renderable Obstacle where
    render renderer (Obstacle height position) =
        drawSquare renderer position height

instance Renderable Game where
    render renderer (InProgress player _ obstacles) = do

        setColour renderer skyblue >> SDL.clear renderer
        setColour renderer green
            >> drawRectangle renderer (0, 0) (windowWidth, groundHeight)
        setColour renderer darkmagenta >> render renderer player
        setColour renderer brown >> mapM_ (render renderer) (visibleObstacles player obstacles)
    render _ (Finished _) = undefined

fontPath :: FilePath
fontPath = "resources/UbuntuMono-B.ttf" 

writeText :: SDL.Window -> Text -> IO ()
writeText window str = do
      font <- SDL.Font.load fontPath 50
      text <- SDL.Font.solid font (SDL.V4 255 255 255 0) str
      SDL.Font.free font
      screen <- SDL.getWindowSurface window
      SDL.surfaceBlit text Nothing screen Nothing
      SDL.freeSurface text
      SDL.updateWindowSurface window

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
