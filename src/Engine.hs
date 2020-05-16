module Engine where

import World
import WorldMap
import ANSI
import Geometry
import Indexable
import Memory

import Control.Monad.State
import System.IO
import Data.IORef
import Data.List.GroupBy
import Data.Bits
import qualified Data.Set as S

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Builder as BU
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.Console.Terminal.Size as T
import qualified Foreign.Ptr      as F
import qualified Foreign.Storable as F
import qualified Foreign.C.String as F


{-# INLINE memoryColor #-}
memoryColor
  = ansiRGB 64 64 64


-- TODO --
--  - Keymaps
--  - Animations
--  - Interupts?
--  - Run


type EngineT m a
   = StateT World m a

type Engine a
   = EngineT IO a

data Event
  = KeyEvent    Char
  | ResizeEvent Size

runEngineT st
  = id
  -- . runMaybeT
  . flip evalStateT st

getScreenSize = do
  liftIO T.size >>= \case
    Just (T.Window h w) -> pure (Size w h)
    Nothing             -> pure (Size 80 80)

setScreenSize s = do
  modify' \st -> st { screen = s }
  redrawAll

engine = do
  liftIO do
    hSetBuffering  stdin    NoBuffering
    hSetBuffering  stdout $ BlockBuffering (Just $ 1024*8)
    hSetBinaryMode stdout   True
    hSetBuffering  stdout $ BlockBuffering Nothing
    hSetEcho       stdout   False
    B.putStr $ smcup <> ansiHideCursor <> ansiClear

  setScreenSize =<< getScreenSize

  fix \loop -> awaitEvent >>= \case
    KeyEvent 'q' -> pure ()
    ev           -> handleEvent ev >> loop

  liftIO do
    B.putStr $ ansiShowCursor <> rmcup

awaitEvent = do
  liftIO (hWaitForInput stdin 20) >>= \case
    True  -> KeyEvent <$> liftIO (hGetChar stdin)
    False -> do
      s1 <- getScreenSize
      s2 <- gets screen
      case s1 /= s2 of
        True  -> pure $ ResizeEvent s1
        False -> awaitEvent

handleEvent = \case
    KeyEvent (charToDir->Just d)
      -> movePlayer d
    KeyEvent '?'
      -> do liftIO . print =<< gets player
            liftIO $ hFlush stdout
    KeyEvent 'r'
      -> do redrawAll
    KeyEvent 'i'
      -> do liftIO $ B.putStr ansiScrollUp
            liftIO $ hFlush stdout
    KeyEvent 'o'
      -> do liftIO $ B.putStr ansiScrollDown
            liftIO $ hFlush stdout
    KeyEvent 'p'
      -> do magicMapping 50
            redrawAll
    ResizeEvent s
      -> setScreenSize s
    _ -> pure ()

charToDir = \case
  'k' -> Just N
  'l' -> Just E
  'j' -> Just S
  'h' -> Just W
  'u' -> Just NE
  'y' -> Just NW
  'n' -> Just SE
  'b' -> Just SW
  _   -> Nothing


movePlayer v = do
  w   <- get
  pl  <- gets $ (+) v . player

  case index (level w) pl of
    Nothing -> pure ()
    Just t
      | isTileSolid t -> pure ()
      | let -> do
          put w { player = pl }
          centerCam pl >>= \case
            False -> partialRedraw
            _ | v == N -> moveNorthFast >> fullRedraw
            _ | v == S -> moveSouthFast >> fullRedraw
            _          -> fullRedraw

centerCam p = do
  Size sw sh <- gets screen
  let p' = p - Pos (div sw 2) (div sh 2)
  w <- get
  case moveCam p' w of
    Nothing    -> pure False
    Just moved -> do
      put w { camera = moved }
      pure True

moveNorthFast = do
  Size w h <- gets screen
  modify' \st -> st
    { buffer = B.replicate w ' ' <> B.take (w*h - w) do buffer st
    }
  liftIO $
    BU.hPutBuilder stdout
      $ ansiCursorHome <> ansiScrollUp

moveSouthFast = do
  Size w h <- gets screen
  modify' \st -> st
    { buffer = B.drop w (buffer st) <> B.replicate w ' '
    }
  liftIO $
    BU.hPutBuilder stdout
      $ ansiSetCursorPos (Pos 0 h) <> ansiScrollDown


-----


doFov = do
  pl   <- gets player
  vd   <- gets viewDist
  v    <- gets camView
  pfov <- gets visible

  let fov = circularFov pl vd $ vFov vd pl v
      dA  = pfov S.\\ fov
      dB  = fov  S.\\ dA
  pure (fov, dA, dB)


redrawAll = do
  Size w h <- gets screen
  modify' \st -> st
    { buffer = B.replicate (w * h) ' '
    }
  liftIO do
    B.putStr $ ansiClear

  centerCam =<< gets player
  fullRedraw

partialRedraw = do
  v              <- gets camView
  st@(fov,dA,dB) <- doFov

  modify' \wm -> wm
    { visible = fov
    -- , buffer  = B.replicate (width v * height v) ' '
    }

  withMemory $ \ptr -> do
    mapM_ (vTransfer v ptr) (S.toList dB)
    renderFOV'Ptr st v ptr

  renderPlayer
  flush

flush = do
  liftIO $ hFlush stdout

renderPlayer = do
  pl <- gets player
  v  <- gets camView

  liftIO do
    setCursorPos $ vClamp v pl
    putChar '@'

-----

withMemory f = do
  v <- gets camView
  liftIO $ vMemPtr v f

-----

bufGetTile c = c .&. 0x01111111
bufGetVis  c = c .&. 0x10000000 /= 0
bufSet v t   = x .|. bufGetTile t
  where
    x = if v then 0x10000000 else 0

circularFov :: Pos -> Int -> S.Set Pos -> S.Set Pos
circularFov (Pos px py) r = S.filter
  \(Pos x y) -> fi r >= sqrt (fi $ (x-px)*(x-px) + (y-py)*(y-py))


-- fullRedraw = do
  -- renderMemory
  -- modify' \w -> w { visible = mempty }
  -- partialRedraw

fullRedraw :: Engine ()
fullRedraw = do
  world <- get :: Engine World
  let v   = camView world
      pl  = player world
      vd  = viewDist world
      fov = circularFov pl vd $ vFov' (viewDist world) (player world) v

  let v@(View (Rect' x y w h) wm) = camView world
  let pts = [ p
            | i <- [0..h-1]
            , o <- [0..w-1]
            , let p = Pos o i
            , p /= pl
            , S.notMember (p + Pos x y) fov
            ]

  str' <- liftIO do
    ref <- newIORef pl
    fs <- B.unsafeUseAsCString (buffer world) \ptr -> do
      vMemPtr v \ptrM -> do
        forM (S.toList fov) \p@(Pos vx vy) -> do
          let ix  = (vy - y)*w + (vx - x)
          let now = level world ! p
          let nowchr = bufSet True $ F.castCharToCChar now

          prd <- F.peekElemOff ptr ix
          vTransfer v ptrM p

          if prd == nowchr then mempty else do
            -- vTransfer v ptrM p
            F.pokeElemOff ptr ix nowchr
            p' <- readIORef ref
            writeIORef ref p
            let prefix = if Pos (vx-1) vy == p'
                            then mempty
                            else ansiSetCursorPos (p - Pos x y)
            pure $ prefix <> BU.charUtf8 now

    let bu' = mconcat fs

    buAny <- newIORef False

    bu <- B.unsafeUseAsCString (buffer world) \ptr -> do
      forM pts \p@(Pos o i) -> do
        let ix = w*i + o
        let vp = p + Pos x y
        prd <- F.peekElemOff ptr ix
        mem <- memGetPos vp (memory wm)
        let memchr = F.castCharToCChar mem
        if not (bufGetVis prd) && prd == memchr then pure mempty else do
          F.pokeElemOff ptr ix memchr
          p' <- readIORef ref
          writeIORef ref p
          writeIORef buAny True
          let prefix = if Pos (o-1) i == p'
                          then mempty
                          else ansiSetCursorPos p
          pure $ prefix <> BU.charUtf8 mem

    ba <- readIORef buAny

    pure
      $ (if not ba then mempty
                   else memoryColor <> mconcat bu <> ansiAttrReset)
     <> bu'
     <> ansiSetCursorPos (vClamp v pl)
     <> BU.charUtf8 '@'

  modify' \wm -> wm
    { visible = fov
    }

  liftIO $ BU.hPutBuilder stdout str'
  flush


renderMemoryPtr world ptr = do
  let
    v@(View (Rect' x y w h) _) = camView world

  forM_ [0..h-1] \i -> do
    vPutLinePos v ptr (Pos x (y + i)) w

renderFOV'Ptr (_, dA, dB) v ptr = do
  unless (S.null dA) do
    BU.hPutBuilder stdout memoryColor
    renderIndiciesPtr v dA ptr
    BU.hPutBuilder stdout ansiAttrReset

  renderIndiciesPtr v dB ptr

renderIndiciesPtr v@(View _ wm) s ptr
  = forM_ (groupBy gf $ S.toList s) \is -> do
      setCursorPos $ vClamp v $ head is
      vPutLinePos v ptr (head is) (length is)
  where
    gf x y = _x x + 1==_x y && _y x == _y y

renderIndiciesPtr' v@(View _ wm) s =
  mconcat <$> forM (groupBy gf s) \is -> do
      let Pos x y = snd $ head is
          l       = length is
      let bu = BU.charUtf8 . fst <$> is
      pure $ ansiSetCursorPos (vClamp v $ Pos x y) <> mconcat bu
  where
    gf (_,x) (_,y) = _x x + 1==_x y && _y x == _y y

renderMemory = do
  liftIO do
    BU.hPutBuilder stdout
      $ ansiSetCursorPos (Pos 0 0)
     <> memoryColor

  world <- get

  withMemory $
    renderMemoryPtr world

  liftIO do
    BU.hPutBuilder stdout $ ansiAttrReset


magicMapping r = do
  Pos x y <- gets player
  lvl     <- gets do tiles . level
  mem     <- gets do memory . level

  let p = Pos (x-r) (y-r)
  let z = focus lvl $ Pos (x-r) (y-r)
  let f = \dt (z, p) -> (focus z dt, p + dt)

  liftIO do
    forM_ (take (r+r) $ iterate (f $ Pos 0 1) (z, p)) \st -> do
      forM_ (take (r+r) $ iterate (f $ Pos 1 0) st) \(z,p) -> do
        let tile = z ! Pos 0 0
        unless (isTileSolid tile) do
          memSetPos p tile mem
