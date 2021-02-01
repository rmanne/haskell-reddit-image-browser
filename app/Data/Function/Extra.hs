{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
  UndecidableInstances #-}

module Data.Function.Extra
  ( (...)
  ) where

class Compose in1 out1 in2 out2 out3 | out1 -> in2, out3 -> out2 where
  (...) :: (in2 -> out2) -> (in1 -> out1) -> in1 -> out3

instance Compose in1 out1 out1 out2 out2 where
  f ... g = f . g

instance {-# INCOHERENT #-} Compose in1 out1 in2 out2 out3 =>
                            Compose in' ((->) in1 out1) in2 out2 ((->) in1 out3) where
  (...) f g a = f ... g a

infixr 9 ...
