module Types(Command(..), Post(..)) where

import qualified Reddit.Types.Post as R

data Command
  = Next
  | Prev
  | Toggle
  | Save
  | ToggleDeleted
  | Remove
  | Refresh
  | Front
  | Back
  | Download R.PostID Int Int [FilePath]
  | Commit
  | FindFailed
  | Status
  | NextImage
  | PrevImage
  | Multi Int
  deriving (Show)

data Post
  = Downloaded R.Post [FilePath]
  | Submitted R.Post
  | Failed R.PostID
  | Deleted R.PostID
  deriving (Show, Read)
