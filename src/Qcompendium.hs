module Qcompendium where

import           Data.Complex (Complex)
import           Data.Map     (Map, findWithDefault, fromList)

class (Eq a, Ord a) =>
      Basis a
  where
  basis :: [a]

data Move
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

data Rotation
  = CtrClockwise
  | Clockwise
  deriving (Eq, Ord, Show)

data Colour
  = Red
  | Yellow
  | Blue
  deriving (Eq, Ord, Show)

instance Basis Bool where
  basis = [False, True]

instance Basis Move where
  basis = [Vertical, Horizontal]

instance Basis Rotation where
  basis = [CtrClockwise, Clockwise]

instance Basis Colour where
  basis = [Red, Yellow, Blue]

type PA = Complex Double

type QV a = Map a PA

qv :: Basis a => [(a, PA)] -> QV a
qv = fromList

-- | Return the probability of amplitude associated with a given unit vector
pr :: Basis a => Map a PA -> a -> PA
pr fm k = findWithDefault 0 k fm

instance Basis Integer where
  basis = [0 ..]

qi :: QV Integer
qi = qv [(i, 1 / fromIntegral i) | i <- basis, i /= 0]

