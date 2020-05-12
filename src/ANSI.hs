module ANSI where

import Geometry
import qualified Data.ByteString.Builder as B
import System.IO (stdout)
import Data.Word


ansiClear      = "\ESC[2J"
ansiFgBlack    = "\ESC[30m"
ansiFgGray     = "\ESC[90m"
ansiAttrReset  = "\ESC[m"
ansiShowCursor = "\ESC[?25h"
ansiHideCursor = "\ESC[?25l"

ansiCursorDown = "\ESC[B"
ansiCursorCol0 = "\ESC[0G"
ansiNL ="\ESC[B" <> "\ESC[0G"



{-# INLINE setCursorPos #-}
setCursorPos (Pos x y)
  = B.hPutBuilder stdout $ "\ESC[" <> B.intDec (y+1) <> B.charUtf8 ';' <> B.intDec (x+1) <> B.charUtf8 'H'

{-# INLINABLE rgb #-}
rgb r g b =
  surround
    ("38;2;" <>
     B.word8Dec r <>
     ";" <>
     B.word8Dec g <>
     ";" <>
     B.word8Dec b)
    "39"

surround open close text
  = do B.hPutBuilder stdout (esc <> open <> m)
       text
       B.hPutBuilder stdout (esc <> close <> m)

esc = "\ESC["
m    = B.charUtf8 'm'
semi = B.charUtf8 ';'

-- rgbFg r g b
--   = B.hPutBuilder stdout $
--       esc <> "38;2;" <> B.word8Dec r <> ";" <> B.word8Dec g <> ";" <> B.word8Dec b
