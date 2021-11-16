module Game 
(
    setupAndRunGame
) where

import System.IO
import Data.Maybe
import Data.Char
import Data.List
import Control.Concurrent
import Data.Array
import System.Console.ANSI

import Game.State
import Game.Render

initGameState :: GameState
initGameState = GameState {
    frameNumber = 0,
    prevInput = None
}

-- =-=-=-=-=-=-=--==--==--==-=--=-=-==-=--==-=-=-=--=-=-=-==--==-==-=-=--==- --

setupAndRunGame :: IO ()
setupAndRunGame = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    setTitle "Rogue Lamb"
    runGame initGameState

runGame :: GameState -> IO ()
runGame prevState = do
    input <- getGameInput
    let nextState = computeNextState prevState input
    hCursorUp stdout $ height mapDimensions -- move cursor back to top
    drawGameState nextState
    threadDelay 696969 -- time in uS
    runGame nextState

getGameInput :: IO (GameInput)
getGameInput = do 
    input <- stdin `ifReadyDo` getChar
    return ( case input of
                Just c  -> buttonMap c
                Nothing -> Game.State.None
           )

-- https://stackoverflow.com/questions/3894792/what-is-a-simple-way-to-wait-for-and-then-detect-keypresses-in-haskell
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

-- TODO: apply button map based on some config
buttonMap :: Char -> GameInput 
buttonMap c
    | toLower c == 'w' = Game.State.Up
    | toLower c == 'a' = Game.State.Left
    | toLower c == 's' = Game.State.Down
    | toLower c == 'd' = Game.State.Right
    | toLower c == ' ' = Game.State.Advance
    | toLower c == 'x' = Game.State.Back
    | toLower c == 'm' = Game.State.Menu
    | otherwise        = Game.State.None

