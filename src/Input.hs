{-# LANGUAGE Arrows #-}
module Input where

import qualified SDL
import SDL.Input.Keyboard.Codes
import FRP.Yampa

data AppInput = AppInput { 
    inputKeyPressed :: Maybe SDL.Scancode,
    inputQuit :: Bool 
    }

initAppInput :: AppInput
initAppInput = AppInput {
    inputKeyPressed = Nothing,
    inputQuit = False 
    }

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp SDL.QuitEvent = inp { inputQuit = True }
nextAppInput inp (SDL.KeyboardEvent ev)
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed
      = inp { inputKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
    | SDL.keyboardEventKeyMotion ev == SDL.Released
      = inp { inputKeyPressed = Nothing }
nextAppInput inp _ = inp


data Command = MoveLeft | MoveRight

parseWinInput :: SF (Event SDL.EventPayload) AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

keyPress :: SF AppInput (Event SDL.Scancode)
keyPress = inputKeyPressed ^>> edgeJust

keyPressed :: SDL.Scancode -> SF AppInput (Event ())
keyPressed code = keyPress >>^ filterE (code ==) >>^ tagWith ()

quitEvent :: SF AppInput (Event ())
quitEvent = arr inputQuit >>> edge

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

parseAppInput :: SF AppInput (Event Command)
parseAppInput = proc input -> do
    leftPressed <- keyPressed ScancodeA -< input
    rightPressed <- keyPressed ScancodeD -< input
    returnA -< lMerge (tag leftPressed MoveLeft) (tag rightPressed MoveRight)