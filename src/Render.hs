module Render where

import Geometry
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Unsafe as B
import qualified Foreign.C.String as F
import qualified Foreign.Ptr      as F
import qualified Foreign.Storable as F
import qualified Foreign.C.Types  as F
import Data.Word
import Foreign.C.Types (CChar (..))


newtype Tile = Tile { unTile :: CChar }
  deriving newtype
    ( Eq, Ord, Enum
    )



