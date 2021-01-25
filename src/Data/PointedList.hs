{-# LANGUAGE TemplateHaskell #-}
module Data.PointedList(PointedList, front, current, back, fromList, goNext, goPrev, goToFront, goToBack, addToFront, addToBack) where

import Control.Lens((&), (%~), makeLenses)

data PointedList a =
  PointedList
    { _front :: [a]
    , _current :: a
    , _back :: [a]
    }
  deriving (Show, Read)

$(makeLenses ''PointedList)

instance Foldable PointedList where
  foldMap f PointedList {..} = foldMap f _front `mappend` f _current `mappend` foldMap f _back

instance Functor PointedList where
  fmap f PointedList {..} = PointedList { _front = fmap f _front, _current = f _current, _back = fmap f _back }

fromList :: [a] -> PointedList a
fromList (head : tail) = PointedList { _front = [], _current = head, _back = tail }

goNext :: PointedList a -> Maybe (PointedList a)
goNext PointedList {_front = f, _current = c, _back = h:t} = Just $ PointedList {_front = c : f, _current = h, _back = t}
goNext PointedList {_back = []} = Nothing

goPrev :: PointedList a -> Maybe (PointedList a)
goPrev PointedList {_front = h:t, _current = c, _back = b} = Just $ PointedList {_front = t, _current = h, _back = c : b}
goPrev PointedList {_front = []} = Nothing

goToFront :: PointedList a -> PointedList a
goToFront list@PointedList {_front = []} = list
goToFront PointedList {..} =
  let first:front' = reverse _front
   in PointedList {_front = [], _current = first, _back = front' ++ _current : _back}

goToBack :: PointedList a -> PointedList a
goToBack list@PointedList {_back = []} = list
goToBack PointedList {..} =
  let last:back' = reverse _back
   in PointedList {_front = back' ++ _current : _front, _current = last, _back = []}

addToFront :: [a] -> PointedList a -> PointedList a
addToFront elements list = list & front %~ (++reverse elements)

addToBack :: [a] -> PointedList a -> PointedList a
addToBack elements list = list & back %~ (++elements)
