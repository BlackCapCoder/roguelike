module Memory where

import Geometry

import qualified Data.Map.Strict as M
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Data.IORef
import System.IO.Unsafe
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.Vector.Storable as VV
import Data.Word
import Data.List (transpose)


type MemoryChunk = V.IOVector CChar

newtype Memory
      = Memory { mref :: IORef (M.Map Pos MemoryChunk) }


chunkSize :: Size
chunkSize
  = Size 256 256

emptyMemory
  = Memory $ unsafePerformIO $ newIORef mempty

getChunk :: Pos -> Memory -> IO MemoryChunk
getChunk p x@(Memory ref) = do
  m <- readIORef ref
  case M.lookup p m of
    Just ch -> pure ch
    Nothing -> do
      ch <- V.replicate (width chunkSize * height chunkSize) 32
      writeIORef ref $ M.insert p ch m
      pure ch

v2vv = uncurry VV.unsafeFromForeignPtr0 . V.unsafeToForeignPtr0
vv2v = uncurry V.unsafeFromForeignPtr0 . VV.unsafeToForeignPtr0

memPtr :: Rect -> Memory -> (Maybe (Ptr CChar) -> IO a) -> IO a
memPtr (Rect' x y w h) mem f
  | mod x sw + w >= sw = f Nothing
  | mod y sh + h >= sh = f Nothing
  | let = do
      ch <- getChunk (V2 cx cy) mem
      V.unsafeWith ch (f . Just)
  where
    Size sw sh = chunkSize
    (cx, mx) = divMod x sw
    (cy, my) = divMod y sh

memGetPos :: Pos -> Memory -> IO Char
memGetPos (V2 x y) mem = do
   ch <- getChunk (V2 cx cy) mem
   fmap (toEnum . fi) $ V.unsafeRead ch i
 where
   Size sw sh = chunkSize
   (cx, mx) = divMod x sw
   (cy, my) = divMod y sh
   i        = my * sw + mx

memSetPos :: Pos -> Char -> Memory -> IO ()
memSetPos (V2 x y) chr mem@(Memory ref) = do
   ch <- getChunk (V2 cx cy) mem
   V.unsafeWith ch \ptr -> pokeElemOff ptr i $ fi $ fromEnum chr
   -- V.unsafeWrite ch i chr
 where
   Size sw sh = chunkSize
   (cx, mx) = divMod x sw
   (cy, my) = divMod y sh
   i        = my * sw + mx

