module View.Types where

import Foreign.C.Types (CInt)

data Action
  = Quit
  | Resize CInt CInt
  | Update String (Maybe FilePath)
