module OldPointedList
  ( PointedList(..)
  , conv
  ) where

import Control.Lens ((&), (.~))
import qualified Data.PointedList as PL

data PointedList a =
  PointedList
    { front :: [a]
    , current :: a
    , back :: [a]
    }
  deriving (Show, Read)

conv :: PointedList a -> PL.PointedList a
conv PointedList {..} =
  let pl = PL.fromList [current]
   in pl & PL.current .~ current & PL.front .~ front & PL.back .~ back
