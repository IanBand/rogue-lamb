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



import Game.Render
import System.Console.ANSI

import Game.State


waterTileRender :: Game.Render.TileRender
waterTileRender = TileRender [Blue] [Cyan, White] ['w', 'W', 'w', 'W'] [True, True, False, False]

waterTile :: Tile
waterTile = Tile waterTileRender Water

screenResolution = mapDimensions

tileCount = (width mapDimensions) * (height mapDimensions)
dummyTileData = array (0, tileCount - 1) (take (tileCount - 1) (zip [0,1..] (repeat waterTile)) )

initGameState = GameState 1 NoInput

-- import System.TimeIt

setupAndRunGame :: IO ()
setupAndRunGame = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    runGame initGameState

runGame :: GameState -> IO ()
runGame prevState = do
    input <- getGameInput
    let nextState = computeNextState prevState input
    hClearScreen stdout
    drawGameState nextState
    threadDelay 300000 --  16666 -- 16.6ms, 60fps TODO: time the compute and draw functions and make this dynamic, also multithreading eventually yadda yadda... 60 fps too fast?
    runGame nextState






-- =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= --



-- https://stackoverflow.com/questions/7829574/simple-counter-in-io
-- define an infinite list of frames to execute...?
-- yes
-- https://stackoverflow.com/questions/39809592/haskell-infinite-recursion-in-list-comprehension?fbclid=IwAR3Kn_m_vxsvwtNyYdhTNThlS3Bgmj9x-Z1Tyytrf9bn9EDH_OgzCTdeW8k

-- https://stackoverflow.com/questions/2472391/how-do-i-clear-the-terminal-screen-in-haskell
-- https://hackage.haskell.org/package/ansi-terminal-0.5.0/docs/System-Console-ANSI.html

-- https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-State-Strict.html no need, probably




-- https://stackoverflow.com/questions/3894792/what-is-a-simple-way-to-wait-for-and-then-detect-keypresses-in-haskell
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

-- TODO: gotta split these up into modules, running into some name colissions
-- TODO: apply button map based on some config
buttonMap :: Char -> GameInput 
buttonMap c
    | toLower c == 'w' = Up
--    | toLower c == 'a' = Left
    | toLower c == 's' = Down
--    | toLower c == 'd' = Right
    | toLower c == ' ' = Advance
    | toLower c == 'x' = Back
    | toLower c == 'm' = MenuInput
    | otherwise = NoInput

getGameInput :: IO (GameInput)
getGameInput = do 
    input <- stdin `ifReadyDo` getChar
    return ( case input of
                Just c  -> buttonMap c
                Nothing -> NoInput
           )




{-
main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    input <- stdin `ifReadyDo` getChar
    if input == Nothing
        then return ()
        else do
            putStr ( [fromJust input] ++ "+")
    -- putStr $ screenBufToString blankBuffer

    setCursorPosition 5 0
    setTitle "ANSI Terminal Short Example"

    setSGR [ SetConsoleIntensity FaintIntensity
           , SetColor Foreground Vivid Red
           ]
    putStr "Hello"

    setSGR [ SetConsoleIntensity FaintIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    putStrLn "World!"
    
    print $ bounds dummyTileData
    threadDelay 1000000 -- 16666 -- 16.6ms, 60fps
    hClearScreen stdout
    
    
    main
-}



