module MCSchematic where

import Protolude
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import Data.NBT
import Data.Serialize
import Linear.V3
import Linear.Vector
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Ix
import Turtle

xyzToMC :: BlockData -> UArray Int32 BlockID
xyzToMC bd = array (0, n) [(getIdx p, bd ! p) | p <- range boundsBd]
  where
    boundsBd@(V3 mx my mz, _) = bounds bd
    (V3 w h l) = (+1) <$> (uncurry $ flip (^-^)) boundsBd
    n = fromIntegral $ rangeSize boundsBd - 1
    getIdx (V3 x y z) = fromIntegral (((y-my)*l + (z-mz))*w + (x-mx)) :: Int32

blockDataToSchematic :: BlockData -> NBT
blockDataToSchematic bd = NBT "Schematic" $ CompoundTag [
      NBT "Width" (ShortTag w)
    , NBT "Height" (ShortTag h)
    , NBT "Length" (ShortTag l)
    , NBT "Materials" (StringTag "Alpha")
    , NBT "Blocks" (ByteArrayTag (xyzToMC bd))
    , NBT "Data" (ByteArrayTag (array (0,n) (zip [0..n] (repeat 0))))
    , NBT "Entities" (ListTag (array (0,-1) []))
    , NBT "TileEntities" (ListTag (array (0,-1) [])) ]
  where
    (V3 w h l) = (+1) <$> fromIntegral <$> (uncurry $ flip (^-^)) (bounds bd)
    n = fromIntegral $ rangeSize (bounds bd) - 1

writeSchematic :: BlockData -> FilePath -> IO ()
writeSchematic bd fp = BL.writeFile fp $ prep $ blockDataToSchematic bd
  where prep n = GZip.compress $ BL.fromChunks [encode n]
