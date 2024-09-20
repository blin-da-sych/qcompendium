module Basis where

import           Data.Complex (Complex)
import           Data.Maybe   (fromMaybe)
import           Prelude      hiding (lookup)

-- | A class 'Basis' that defines an abstract representation for a basis set.
--   The types that are instances of this class represent possible basis vectors
--   that can be used in quantum vector spaces.
class (Eq a, Ord a) =>
      Basis a
  -- | 'basis' is a list of possible values or "basis vectors" for the type 'a'.
  where
  basis :: [a]

-- | The 'Move' data type represents two possible movements: Vertical and Horizontal.
data Move
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

-- | The 'Rotation' data type represents two possible rotations:
--   Counter-clockwise ('CtrClockwise') and 'Clockwise'.
data Rotation
  = CtrClockwise
  | Clockwise
  deriving (Eq, Ord, Show)

-- | The 'Colour' data type represents three colours: 'Red', 'Yellow', and 'Blue'.
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

-- | Type alias 'PA' representing a **probability amplitude**, which is
--   a complex number.
type PA = Complex Double

-- | 'AssocList' represents an association list of key-value pairs. It's
--   used instead of 'Map' due to its laziness, which is not guaranteed by 'Map'.
type AssocList k v = [(k, v)]

-- | 'QV' is a type alias for a **quantum vector space** (represented by a map).
--   The map keys are basis vectors (of type 'a') and the values are complex
--   probability amplitudes of type 'PA'.
type QV a = AssocList a PA

-- | 'lookup' searches for a key in an 'AssocList'. If the key is found,
--   it returns 'Just' the associated value, otherwise it returns 'Nothing'.
lookup :: Eq k => k -> AssocList k v -> Maybe v
lookup _ [] = Nothing
lookup key ((k, v):xs)
  | key == k = Just v
  | otherwise = lookup key xs

-- | 'qVector' constructs a **quantum vector** ('QV') from a list of basis
--   vector-amplitude pairs. The 'Basis' constraint ensures that only types
--   with a well-defined basis can be used.
qVector :: Basis a => [(a, PA)] -> QV a
qVector = id

-- | 'amplitude' returns the **probability amplitude** associated with a given unit
--   vector in the quantum vector. If the vector is not found, it defaults to '0'.
amplitude :: Basis a => AssocList a PA -> a -> PA
amplitude qvs k = fromMaybe 0 (lookup k qvs)

-- | Instance of 'Basis' for 'Integer'. The basis vectors are non-negative
--   integers (starting from 0).
instance Basis Integer where
  basis = [0 ..]

-- | 'qInteger' represents an infinite quantum vector for 'Integer', where the
--   amplitude for each integer is given by the reciprocal of the integer
--   (except for 0). The laziness of 'AssocList' allows this to be defined
--   for an infinite set of integers.
qInteger :: QV Integer
qInteger = qVector [(i, 1 / fromIntegral i) | i <- tail basis]
