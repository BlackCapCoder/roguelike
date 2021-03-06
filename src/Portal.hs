module Portal where

import Geometry
import qualified Sized as S
import FOV as F
import qualified Data.IntMap as M
import Indexable


-- newtype PMap = PMap
--   { unPMap :: M.IntMap (SomeIndexable Pos Char) }


-- ex1 = PMap $ M.fromList
--   [ (1,) do Ix outside
--   , (2,) do Ix inside
--   ]


outside :: S.Chunk 50 11
outside
  = S.Chunk
    ".......==================.......==================\
    \..=====================....=====================..\
    \======............................................\
    \.........########............2....................\
    \.........########..................##..##.........\
    \.........########..................#2222#.........\
    \.........##2222##..................#2222#.........\
    \.........###..###..................##..##.........\
    \..................................................\
    \.....................................!............\
    \.............'...................................."

inside :: S.Chunk 50 11
inside
  = S.Chunk
    "             #.#                      #.#         \
    \             #.#                      #.#         \
    \   ###  ######/########      1   ######/########  \
    \   #.#  #.............#     #.#  #.............#  \
    \   #.####.........<...#######.####.............###\
    \   #......$.......................................\
    \   ###............................................\
    \     ###/###..####..#############/###..####..#####\
    \       #.# 1111  #..#           #.# 1111  #..#    \
    \       ###       #..#           ###       #..#    \
    \                 #..#                     #..#    "
