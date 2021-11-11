module Main where

import System.IO
import Data.Maybe
import Data.Array
import Control.Concurrent
import System.Console.ANSI


-- import System.TimeIt

data TextBoxPosition = Top | Middle | Bottom deriving (Show)

data TextBox = TextBox {
    -- charPerFrame :: Natural, -- we aint usin this our first time round... must keep in mind frames will only be rendered upon user input (wasd + numbers)
    contents :: String,
    position :: TextBoxPosition
} deriving (Show)

data MapDimensions = MapDimensions {
    height :: Int,
    width  :: Int
} deriving (Show)
mapDimensions = MapDimensions 64 64
screenResolution = mapDimensions

type Coordinate = (Int, Int)
coordinateToIndex :: Coordinate -> Int
coordinateToIndex (x,y) = (width mapDimensions) * y + x

tileCount = (width mapDimensions) * (height mapDimensions)
tileData = array (0, tileCount - 1) [(i, i) | i <- [0 .. (tileCount - 1)]]

data MapId = Home | RivalHome | StartingTown | StartLab deriving(Show, Eq)

data ColissionType = Ground | Encounter | Wall | Warp MapId Coordinate | Water deriving(Show)

data TileRender = TileRender {
    -- global frame number mod list length... or they could be infinite lists??? nah
    charColorList :: [System.Console.ANSI.Color],
    backgroundColorList :: [System.Console.ANSI.Color],
    characterList   :: [Char],
    italicizedList  :: [Bool]
} deriving (Show)
data Tile = Tile {
    render :: TileRender,
    colission :: ColissionType
} deriving (Show)

data SceneType = InMenu | InTextBox | InOverworld | InBattle deriving(Show)

data Cardinal = North | South | East | West deriving(Show)

data OverworldState = OverworldState {
    playerFacing :: Cardinal,
    playerCoordinate :: Coordinate,
    currentMap :: MapId
    -- list of current actors + positions?
} deriving(Show)
data GameState = GameState {
    currentSceneStack :: [SceneType], -- use a real stack template or whatever its called (ADT?) in haskell
    overworldState :: OverworldState
    -- team :: (Ally, Ally, Ally, Ally, Ally),
    -- inventory :: (Item, Item, Item, Item ... ),
    -- 
    -- battleState :: BattleState,
    -- menuState :: MenuState,
    -- textBoxState :: TextBoxState,
    -- gameFlags :: GameFlags

} deriving (Show)

data GameInput = Up | Down | Left | Right | Advance | Back | MenuInput deriving(Show)
-- data VirtualInput ... do we need this layer?

computeNextState :: GameState -> GameInput -> GameState
computeNextState prevState input = prevState

drawState :: GameState -> [IO ()]
drawState state = [return ()]


-- drawTextBox :: ScreenBuf -> TextBox -> ScreenBuf

{-
blankBuffer = blankBufferNxN 16 16
blankBufferNxN :: Int -> Int -> ScreenBuf
blankBufferNxN h w
    | h <= 0 = blankBufferNxN 64 w
    | w <= 0 = blankBufferNxN h 64
    | otherwise = replicate w col where col = replicate h '.'


screenBufToString :: ScreenBuf -> [Char]
screenBufToString  buf = (foldl1 (\acc line -> acc ++ "\n" ++ line) buf) ++ "\n"
--printScreenBuf buf = map (\l -> l ++ "\n") buf 

-}


-- =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= --


ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

-- https://stackoverflow.com/questions/3894792/what-is-a-simple-way-to-wait-for-and-then-detect-keypresses-in-haskell
-- https://stackoverflow.com/questions/7829574/simple-counter-in-io
-- define an infinite list of frames to execute...?

-- https://stackoverflow.com/questions/2472391/how-do-i-clear-the-terminal-screen-in-haskell
-- https://hackage.haskell.org/package/ansi-terminal-0.5.0/docs/System-Console-ANSI.html


    
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
    
    print $ bounds tileData
    threadDelay 1000000 -- 16666 -- 16.6ms, 60fps
    hClearScreen stdout
    
    
    main




