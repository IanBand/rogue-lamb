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

waterTileRender :: Game.Render.TileRender
waterTileRender = TileRender {
    charColorList = [Blue],
    backgroundColorList = [Cyan, White],
    characterList = ['w', 'W', 'w', 'W'],
    italicizedList = [True, True, False, False]
} 

waterTile :: Tile
waterTile = Tile {
    render = waterTileRender,
    colission = Water
}

dummyTileData :: Array Int Tile
dummyTileData = array arrayBounds (take (Game.State.tileCount - 1) (zip [0,1..] (repeat waterTile)) )

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
    hClearScreen stdout
    drawGameState nextState
    threadDelay 300000 -- time in uS
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

-- TODO: gotta split these up into modules, running into some name colissions
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

