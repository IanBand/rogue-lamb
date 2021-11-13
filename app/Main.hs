module Main where

import System.IO
import Data.Maybe
import Data.Array
import Data.Char
import Data.List
import Control.Concurrent
import System.Console.ANSI



-- import System.TimeIt

data TextBoxPosition = Top | Middle | Bottom deriving (Show)

data TextBox = TextBox {
    -- charPerFrame :: Natural, -- we aint usin this our first time round... must keep in mind frames will only be rendered upon user input (wasd + numbers)
    contents :: String,
    position :: TextBoxPosition
} deriving (Show)


-- Tile Stuff 

data TileRender = TileRender {
    -- global frame number mod list length... or they could be infinite lists??? nah
    charColorList :: [System.Console.ANSI.Color],
    backgroundColorList :: [System.Console.ANSI.Color],
    characterList   :: [Char],
    italicizedList  :: [Bool]
} deriving (Show)

waterTileRender = TileRender [Blue] [Cyan, White] ['w', 'W', 'w', 'W'] [True, True, False, False]

data ColissionType = Ground | Encounter | Wall | Warp MapId Coordinate | Water deriving(Show)

data Tile = Tile {
    render :: TileRender,
    colission :: ColissionType
} deriving (Show)

waterTile = Tile waterTileRender Water

-- Map Stuff

data MapDimensions = MapDimensions { -- maybe rename to GlobalMapDimensions
    height :: Int,
    width  :: Int
} deriving (Show)
mapDimensions = MapDimensions 64 64
screenResolution = mapDimensions

type Coordinate = (Int, Int)
coordinateToIndex :: Coordinate -> Int
coordinateToIndex (x,y) = (width mapDimensions) * y + x

tileCount = (width mapDimensions) * (height mapDimensions)
dummyTileData = array (0, tileCount - 1) (take (tileCount - 1) (zip [0,1..] (repeat waterTile)) )

type TileBuffer = Array Int Tile -- pretty sure this is correct?


data MapId = Home | RivalHome | StartingTown | StartLab deriving(Show, Eq)

data MapStructure = MapStructure {
    id :: MapId,
    tileData :: TileBuffer 
    -- some sort of actor list, an actors existance may depend on game flags
} deriving(Show)





data SceneType = InMenu | InTextBox | InOverworld | InBattle deriving(Show)

data Cardinal = North | South | East | West deriving(Show)

data OverworldState = OverworldState {
    playerFacing :: Cardinal,
    playerCoordinate :: Coordinate,
    currentMap :: MapId
    -- list of current actors + positions?
} deriving(Show)
data GameState = GameState {
    frameNumber :: Int,
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

data GameInput = Up | Down | Left | Right | Advance | Back | MenuInput | NoInput deriving(Show)
-- data PhysicalInput = all the key presses
-- a user-configurable map from physical input to GameInput
-- buttonMap :: PhysicalInput -> ButtonMapConfig -> GameInput

computeNextState :: GameState -> GameInput -> GameState
computeNextState prevState input = prevState



-- Int args are for the current frame, this could also be in the subStates or we could just pass the global state
drawOverworld :: TileBuffer {- -> OverworldState -> int -} -> TileBuffer
drawOverworld buff = buff

drawBattle :: TileBuffer {- -> BattleState -> int -} -> TileBuffer
drawBattle buff = buff

drawTextBox :: TileBuffer {- -> TextBoxState -> int -} -> TileBuffer
drawTextBox buff = buff

drawMenu :: TileBuffer {- -> TextBoxState -> int -} -> TileBuffer
drawMenu buff = buff

-- drawActors/Player

drawTileBuffer :: TileBuffer -> IO ()
drawTileBuffer buff = mapM_ print [1,2,3]

drawGameState :: GameState -> IO ()       -- this is gonna be BIG, need to break it down, find the intermediate representations, ect
drawGameState state = mapM_ print [1,2,3] -- goal of this function is to make a fat list of setSGR and putChar (as seen in the current main) then use mapM to make it into one IO action

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


executeFrame :: IO (GameState) -> IO (GameState)
executeFrame prevState = do
    prevState' <- prevState
    input <- getGameInput
    nextState <- return $ computeNextState prevState' input
    drawGameState nextState
    threadDelay 16666 -- 16.6ms, 60fps TODO: time the compute and draw functions and make this dynamic, also multithreading eventually yadda yadda...
    return nextState

    

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




