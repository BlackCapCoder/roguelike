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
  -- = Size 64 64
  -- = Size 16 16

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

-- memPtr :: Rect -> Memory -> (Maybe (Ptr CChar) -> IO a) -> IO a
-- memPtr (Rect' x y w h) mem f
--   | mod x sw + w >= sw = f Nothing
--   | mod y sh + h >= sh = f Nothing
--   | let = do
--       ch <- getChunk (V2 cx cy) mem
--       V.unsafeWith ch (f . Just)
--   where
--     Size sw sh = chunkSize
--     (cx, mx) = divMod x sw
--     (cy, my) = divMod y sh


-- memPtr :: Rect -> Memory -> (Ptr CChar -> IO a) -> IO a
-- memPtr (Rect' x y w h) mem f
--   | mod x sw + w >= sw = do
--       vl <- getChunk (V2 cx cy) mem
--       vr <- getChunk (V2 (cx+1) cy) mem
--       let v = mconcat $ concat [ v2vv <$> [V.unsafeSlice (y*sw) sw vl, V.unsafeSlice (y*sw) sw vr]
--                       | y <- [0..sh-1] ]
--       res <- VV.unsafeWith v f
--       let (ls,rs) = mconcat [ ( VV.unsafeSlice (y*2*sw) sw v
--                               , VV.unsafeSlice (y*2*sw+sw) sw v)
--                             | y <- [0..sh-1] ]
--       let vl'' = vv2v ls
--       let vr'' = vv2v rs
--       modifyIORef' (mref mem)
--         $ M.insert (V2 cx cy) vl''
--         . M.insert (V2 (cx+1) cy) vr''
--
--       pure res
--   | mod y sh + h >= sh = do
--       vl <- getChunk (V2 cx cy) mem
--       vr <- getChunk (V2 cx (cy+1)) mem
--       let v = VV.concat $ map v2vv [vl,vr]
--       res <- VV.unsafeWith v f
--       let [vl',vr'] = map vv2v [ VV.unsafeSlice 0 (sw*sh) v
--                                , VV.unsafeSlice (sw*sh) (sw*sh) v ]
--       modifyIORef' (mref mem)
--         $ M.insert (V2 cx cy) vl'
--         . M.insert (V2 cx (cy+1)) vr'
--       pure res
--   | let = do
--       ch <- getChunk (V2 cx cy) mem
--       V.unsafeWith ch f
--   where
--     Size sw sh = chunkSize
--     (cx, mx) = divMod x sw
--     (cy, my) = divMod y sh


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
