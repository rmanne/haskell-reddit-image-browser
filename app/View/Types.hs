module View.Types
  ( Action(..)
  ) where

import Foreign.C.Types (CInt)
import SDL (V2)

data Action
  = Quit
  | Resize (V2 CInt)
  | Update String (Maybe FilePath)
