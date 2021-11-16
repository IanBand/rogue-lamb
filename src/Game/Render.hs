module Game.Render where
{-
(
    drawGameState
    ,System.Console.ANSI.Color
    ,Tile
    ,TileRender
)
-} 

import Game.State

import Data.Array
import System.Console.ANSI


type TileBuffer = Array Int Tile -- pretty sure this is correct?


data Tile = Tile {
    render :: TileRender,
    colission :: ColissionType
} deriving (Show)

data MapStructure = MapStructure {
    id :: MapId,
    tileData :: TileBuffer 
    -- some sort of actor list, an actors existance may depend on game flags
} deriving(Show)


data TileRender = TileRender {
    -- global frame number mod list length... or they could be infinite lists??? nah
    charColorList :: [System.Console.ANSI.Color],
    backgroundColorList :: [System.Console.ANSI.Color],
    characterList   :: [Char],
    italicizedList  :: [Bool]
} deriving (Show)





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

drawGameState :: GameState -> IO ()  -- this is gonna be BIG, need to break it down, find the intermediate representations, ect
drawGameState state = print state    -- goal of this function is to make a fat list of setSGR and putChar (as seen in the commented out main) then use mapM to make it into one IO action
    
