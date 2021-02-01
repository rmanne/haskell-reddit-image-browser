module View.Types
  ( Action(..)
  ) where

import Foreign.C.Types (CInt)

data Action
  = Quit
  | Resize CInt CInt
  | Update String (Maybe FilePath)
