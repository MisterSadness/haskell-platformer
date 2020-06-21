{-# LANGUAGE Arrows #-}
module Input where

import qualified SDL
import SDL.Input.Keyboard.Codes
import FRP.Yampa

data AppInput = AppInput {
    inputKey :: Maybe SDL.Scancode,
    inputMotion :: Maybe SDL.InputMotion,
    inputQuit :: Bool
    }

emptyAppInput :: AppInput
emptyAppInput = AppInput {
    inputKey = Nothing,
    inputMotion = Nothing,
    inputQuit = False
    }

data InputAction = Pressed Command | Released Command
data Command = MoveLeft | MoveRight | Jump | StopLeft | StopRight deriving Enum

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp SDL.QuitEvent = inp { inputQuit = True }
nextAppInput inp (SDL.KeyboardEvent ev)
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed
      = inp { inputKey = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev, inputMotion = Just SDL.Pressed }
    | SDL.keyboardEventKeyMotion ev == SDL.Released
      = inp { inputKey = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev, inputMotion = Just SDL.Released }
nextAppInput inp _ = inp

parseWinInput :: SF (Event SDL.EventPayload) AppInput
parseWinInput = accumHoldBy nextAppInput emptyAppInput

keyPress :: SF AppInput (Event SDL.Scancode)
keyPress = inputKey ^>> edgeJust

keyPressed :: SDL.Scancode -> SF AppInput (Event ())
keyPressed code = keyPress >>^ filterE (code ==) >>^ tagWith ()

quitEvent :: SF AppInput (Event ())
quitEvent = arr inputQuit >>> edge

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

parseAppInput :: SF AppInput (Event Command)
parseAppInput = arr parse
    where 
        parse AppInput{inputKey = Just ScancodeA, inputMotion = Just SDL.Pressed} = Event MoveLeft
        parse AppInput{inputKey = Just ScancodeD, inputMotion = Just SDL.Pressed} = Event MoveRight
        parse AppInput{inputKey = Just ScancodeSpace, inputMotion = Just SDL.Pressed} = Event Jump
        parse AppInput{inputKey = Just ScancodeA, inputMotion = Just SDL.Released} = Event StopLeft
        parse AppInput{inputKey = Just ScancodeD, inputMotion = Just SDL.Released} = Event StopRight
        parse _ = NoEvent
