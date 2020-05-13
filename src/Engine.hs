module Engine where

import World
import WorldMap
import ANSI
import Geometry
import Indexable

import Control.Monad.State
import System.IO
import Data.IORef
import Data.List.GroupBy
import qualified Data.Set as S

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified System.Console.Terminal.Size as T
import qualified Foreign.Ptr      as F
import qualified Foreign.Storable as F
import qualified Foreign.C.String as F


-- TODO --
--  - Keymaps
--  - Animations
--  - Interupts?
--  - Run


type EngineT m a
   = StateT World m a

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
  liftIO . B.putStr $ ansiClear
  centerCam =<< gets player
  redraw

engine = do
  liftIO do
    hSetBuffering  stdin    NoBuffering
    hSetBuffering  stdout $ BlockBuffering (Just $ 1024*8)
    hSetBinaryMode stdout   True
    hSetBuffering  stdout $ BlockBuffering Nothing
    hSetEcho       stdout   False
    B.putStr $ ansiHideCursor <> ansiClear

  setScreenSize =<< getScreenSize

  fix \loop -> awaitEvent >>= \case
    KeyEvent 'q' -> pure ()
    ev           -> handleEvent ev >> loop

  liftIO do
    B.putStr ansiShowCursor

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
      -> do liftIO $ B.putStr $ ansiClear
            redraw
            liftIO $ hFlush stdout
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
            False -> renderFOV
            True  -> redraw

centerCam p = do
  Size sw sh <- gets screen
  let p' = p - Pos (div sw 2) (div sh 2)
  w <- get
  case moveCam p' w of
    Nothing    -> pure False
    Just moved -> do
      put w { camera = moved }
      pure True

redraw = do
  modify' \w -> w { visible = mempty }
  renderMemory
  renderFOV

renderMemory = do
  Rect' x y w h <- gets getCam
  Size sw _     <- gets screen
  v             <- gets camView

  liftIO $ vMemPtr v \ptr -> do
    memoryColor do
      setCursorPos (Pos 0 0)
      forM_ [0..h-1] \i -> do
        vPutLinePos v ptr (Pos x (y + i)) w
        when (w /= sw) $ B.putStr ansiNL


renderFOV = do
  lvl  <- gets level
  pl   <- gets player
  vd   <- gets viewDist
  v    <- gets camView
  pfov <- gets visible

  let fov = vFov vd pl v
      dA  = pfov S.\\ fov
      dB  = fov  S.\\ dA

  modify' \wm -> wm { visible = fov }

  liftIO $
    vMemPtr v \ptrM -> do
      -- B.putStr ansiClear

      mapM_ (vTransfer v ptrM) (S.toList dB)

      unless (S.null dA) do
        memoryColor do
          renderIndicies v dA ptrM

      renderIndicies v dB ptrM

  liftIO do
    setCursorPos $ vClamp v pl
    putChar '@'
    -- putStr $ show do length dA + length dB
    hFlush stdout

renderIndicies v@(View _ wm) s ptr
  = forM_ (groupBy gf $ S.toList s) \is -> do
      setCursorPos $ vClamp v $ head is
      vPutLinePos v ptr (head is) (length is)
  where
    gf x y = _x x + 1==_x y && _y x == _y y

memoryColor
  = rgb 64 64 64


