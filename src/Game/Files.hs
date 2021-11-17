module Game.Files where

import Game.State
import Game.Render

loadMap :: MapId -> IO (MapStructure)
loadMap id = return(MapStructure {
    Game.Render.id = WaterTest,
    tileData = waterTileBuffer
    -- some sort of actor list, an actors existance may depend on game flags
})

writeMap :: MapStructure -> IO ()
writeMap map = return ()