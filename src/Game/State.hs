module Game.State where

{-
(
     mapDimensions
    ,computeNextState
    ,GameInput
    ,GameState
    ,ColissionType
    ,MapId
)
-}


data MapId = Home | RivalHome | StartingTown | StartLab | WaterTest deriving(Show, Eq)

data TextBoxPosition = Top | Middle | Bottom deriving (Show)

data TextBox = TextBox {
    -- charPerFrame :: Natural, -- we aint usin this our first time round... must keep in mind frames will only be rendered upon user input (wasd + numbers)
    contents :: String,
    position :: TextBoxPosition
} deriving (Show)


-- Tile Stuff 

data ColissionType = Ground | Encounter | Wall | Warp MapId Coordinate | Water deriving(Show)

-- Map Stuff

data MapDimensions = MapDimensions { -- maybe rename to GlobalMapDimensions
    height :: Int,
    width  :: Int
} deriving (Show)

mapDimensions :: MapDimensions
mapDimensions = MapDimensions { height = 32, width = 64 }

tileCount :: Int
tileCount = (width mapDimensions) * (height mapDimensions)

arrayBounds :: (Int, Int)
arrayBounds = (0, tileCount - 1)

type Coordinate = (Int, Int)
coordinateToIndex :: Coordinate -> Int
coordinateToIndex (x,y) = (width mapDimensions) * y + x

indexToCoordinate :: Int -> Coordinate
indexToCoordinate i =   (x,y)
                        where x = i `mod` (width mapDimensions)
                              y = i `div` (width mapDimensions)






data SceneType = InMenu | InTextBox | InOverworld | InBattle deriving(Show)

data Cardinal = North | South | East | West deriving(Show)

data OverworldState = OverworldState {
    playerFacing :: Cardinal,
    playerCoordinate :: Coordinate,
    currentMap :: MapId
    -- list of current actors + positions?
} deriving(Show)

data GameInput = Up | Down | Left | Right | Advance | Back | Menu | None deriving(Show)


data GameState = GameState {
    frameNumber :: Int,
    prevInput :: GameInput
    -- currentSceneStack :: [SceneType], -- use a real stack template or whatever its called (ADT?) in haskell
    -- overworldState :: OverworldState
    -- team :: (Ally, Ally, Ally, Ally, Ally),
    -- inventory :: (Item, Item, Item, Item ... ),
    -- 
    -- battleState :: BattleState,
    -- menuState :: MenuState,
    -- textBoxState :: TextBoxState,
    -- gameFlags :: GameFlags

} deriving (Show)



-- data PhysicalInput = all the key presses
-- a user-configurable map from physical input to GameInput
-- buttonMap :: PhysicalInput -> ButtonMapConfig -> GameInput

computeNextState :: GameState -> GameInput -> GameState
computeNextState prevState input = GameState {
    frameNumber = frameNumber prevState + 1,
    prevInput = input
}
