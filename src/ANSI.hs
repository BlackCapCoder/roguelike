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
ansiScrollDown = "\ESCD"
ansiScrollUp   = "\ESCM"

ansiCursorDown = "\ESC[B"
ansiCursorCol0 = "\ESC[0G"
ansiNL         = "\ESC[B\ESC[0G"
ansiCursorHome = "\ESC[H"


ansiSetCursorPos (Pos x y)
  = mconcat
  [ esc
  , B.intDec (y+1), semi
  , B.intDec (x+1)
  , B.charUtf8 'H'
  ]

{-# INLINE setCursorPos #-}
setCursorPos p
  = B.hPutBuilder stdout $ ansiSetCursorPos p

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

surround open close ctx
  = do B.hPutBuilder stdout (esc <> open <> m)
       r <- ctx
       B.hPutBuilder stdout (esc <> close <> m)
       pure r

esc = "\ESC["
m    = B.charUtf8 'm'
semi = B.charUtf8 ';'


smcup = "\ESC[?1049h" -- alternate buffer
rmcup = "\ESC[?1049l" -- original buffer

-- https://real-world-systems.com/docs/ANSIcode.html#scrollregion

ansiRGB r g b
  = mconcat
  [ esc, "38;2;"
  , B.word8Dec r, semi
  , B.word8Dec g, semi
  , B.word8Dec b, m
  ]
