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

import Data.Char -- temp


type TileBuffer = Array Int Tile -- pretty sure this is correct?


data Tile = Tile {
    render :: TileRender,
    colission :: ColissionType
} deriving (Show)

-- TODO: MapStructure should really be defined in state, along with some other things, State and Render should both import from another module... cant really decide on the name/exact functionality
-- another note is that I am kind of mixing colission data and render data. I think storing the tileset and colission in seperate arrays might solve this
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




waterTileRender :: Game.Render.TileRender
waterTileRender = TileRender {
    charColorList = [Cyan],
    backgroundColorList = [Blue],
    characterList = ['w', 'W', 'w', 'W'],
    italicizedList = [True, True, False, False]
} 

waterTile :: Tile
waterTile = Tile {
    render = waterTileRender,
    colission = Water
}
waterTileBuffer :: TileBuffer
waterTileBuffer = solidTileBuffer waterTile

solidTileBuffer :: Tile -> TileBuffer
solidTileBuffer tile = array arrayBounds (take tileCount (zip [0,1..] (repeat tile) ) )

-- Terminology note: functions that render produce a TileBuffer, while functions that draw actually produce IO commands to draw a tilebuffer


renderGameState :: GameState -> TileBuffer
renderGameState state = solidTileBuffer waterTile


renderOverworld :: TileBuffer {- -> OverworldState -> int -} -> TileBuffer
renderOverworld buff = buff

renderBattle :: TileBuffer {- -> BattleState -> int -} -> TileBuffer
renderBattle buff = buff

renderTextBox :: TileBuffer {- -> TextBoxState -> int -} -> TileBuffer
renderTextBox buff = buff

renderMenu :: TileBuffer {- -> TextBoxState -> int -} -> TileBuffer
renderMenu buff = buff

-- drawActors/Player

drawTile :: Tile -> Int -> [IO ()]
drawTile (Tile render colission) keyframe = [
                                setSGR [
                                      SetColor Background Vivid $ currentValueIn backgroundColorList
                                     ,SetColor Foreground Vivid $ currentValueIn charColorList
                                     ,SetItalicized $ currentValueIn italicizedList
                                 ]
                                ,putChar $ currentValueIn characterList
                                ]
                                where currentValueIn = valueOnFrame render keyframe
    
valueOnFrame :: TileRender -> Int -> (TileRender -> [a]) -> a
valueOnFrame render keyframe access  = (access render) !! ( keyframe `mod` (length $ access render) )

-- put in a newline if the index tells us we are at the end of a line 
-- TODO: fix this logic
drawRow :: (Int, Tile) -> Int -> [IO ()]
drawRow (index, tile) frameNumber = if index `mod` (width Game.State.mapDimensions) == 0 
                                    then [putChar '\n'] ++ (drawTile tile $ frameNumber + checker) 
                                    else (drawTile tile $ frameNumber + checker)

                                    where checker = (if(x `mod` 2 == 0) /= (y `mod` 2 == 0) then 1 else 0)
                                          (x,y) = indexToCoordinate index 

drawTileBuffer :: TileBuffer -> Int -> IO ()
drawTileBuffer buff frameNumber = sequence_ $ concat $ map (`drawRow` frameNumber) (assocs buff) 



-- this is gonna be BIG, need to break it down, find the intermediate representations, ect
-- goal of this function is to make a fat list of setSGR and putChar (as seen in the commented out main) then use mapM to make it into one IO action
drawGameState :: GameState -> IO ()  
drawGameState state = drawTileBuffer waterTileBuffer (frameNumber state) 
