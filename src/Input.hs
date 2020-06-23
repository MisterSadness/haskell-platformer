{-# LANGUAGE Arrows #-}
-- | This module defines all functions used to parse user's input into types relevant to the game
module Input(
    AppInput(..),
    Command(..),
    parseWinInput,
    parseAppInput,
    handleExit
) where

import qualified SDL
import SDL.Input.Keyboard.Codes
import FRP.Yampa

-- | Type containing only the relevant information from SDL's EventPayload
data AppInput = AppInput {
    inputKeyPressed :: Maybe SDL.Scancode,
    inputQuit :: Bool
    }

-- | Command that the user can issue
data Command = MoveLeft | MoveRight | Jump

-- | Function parsing SDL.EventPayload into AppInput
parseWinInput :: SF (Event SDL.EventPayload) AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

-- | Function parsing AppInput into Commands
parseAppInput :: SF AppInput (Event Command)
parseAppInput = proc input -> do
    leftPressed <- keyPressed ScancodeA -< input
    rightPressed <- keyPressed ScancodeD -< input
    spacePressed <- keyPressed ScancodeSpace -< input
    returnA -< mergeEvents [tag leftPressed MoveLeft, tag rightPressed MoveRight, tag spacePressed Jump]

-- | Function detecting user closing the app, to enable graceful exit, true when the process should end
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- helper functions, not exported
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

keyPress :: SF AppInput (Event SDL.Scancode)
keyPress = inputKeyPressed ^>> edgeJust

keyPressed :: SDL.Scancode -> SF AppInput (Event ())
keyPressed code = keyPress >>^ filterE (code ==) >>^ tagWith ()

quitEvent :: SF AppInput (Event ())
quitEvent = arr inputQuit >>> edge



