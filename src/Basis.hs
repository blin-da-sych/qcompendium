{-|
This module defines the 'Basis' class and its instances, representing the possible
basis vectors for various types that can be used in quantum vector spaces. Unlike
the lazy version, this implementation uses strict data structures such as 'Map' to
enforce unique basis vectors and strict evaluation. This ensures that no duplicate
basis vectors are allowed, and operations on quantum vectors are performed in a
strict manner, making it more suitable for finite, concrete basis sets.
-}
module Basis where

import           Data.Complex (Complex, magnitude)
import           Data.Map     (Map, elems, fromList, lookup)
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

instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]

-- | Type alias 'PA' representing a __probability amplitude__, which is
--   a complex number.
type PA = Complex Double

-- | 'QV' is a type alias for a __quantum vector space__ (represented by a map).
--   The map keys are basis vectors (of type 'a') and the values are complex
--   probability amplitudes of type 'PA'.
type QV a = Map a PA

-- | 'qVector' constructs a __non-normalized quantum vector__ ('QV') from a Map of
--   basis vector-amplitude pairs.
--
--   __Parameters:__
--
--   - Map of pairs @Map a PA@, where 'a' is the basis vector and 'PA' is
--     the probability amplitude (a complex number)
--
--   __Returns:__
--
--   - @QV a@: a quantum vector, represented as a Map of
--     basis vectors and probability amplitudes
qVector :: Basis a => [(a, PA)] -> QV a
qVector = fromList

-- | 'isNormalized' checks if a quantum vector is normalized.
--   A normalized vector must have the sum of squared magnitudes of its
--   probability amplitudes equal to 1 (within a small tolerance).
--   The check examines all elements in the 'Map', where the keys represent
--   basis states and the values represent the corresponding probability amplitudes.
--
--   __Parameters:__
--
--   - A 'Map' of keys 'a' (basis states) and probability amplitudes 'PA'
--
--   __Returns:__
--
--   - 'Bool': 'True' if the vector is normalized, otherwise 'False'
isNormalized :: (Basis a, Ord a) => Map a PA -> Bool
isNormalized m = abs (sum (map (square . magnitude) (elems m)) - 1) < 1e-9
  where
    square = (^ (2 :: Int))

-- | 'qVector' constructs a __normalized quantum vector__ ('QV') from a Map of
--   basis vector-amplitude pairs. It ensures the vector is normalized.
--
--   __Parameters:__
--
--   - Map of pairs @Map a PA@, where 'a' is the basis vector and 'PA' is
--     the probability amplitude (a complex number)
--
--   __Returns:__
--
--   - @QV a@: a quantum vector, represented as a Map of
--     basis vectors and probability amplitudes
--
--   __Throws:__
--
--   - An 'error' with the message *"The quantum vector is not normalized."*
--     if the sum of squared magnitudes of the probability amplitudes does not equal 1.
qVector' :: Basis a => [(a, PA)] -> QV a
qVector' qv
  | (isNormalized . fromList) qv = fromList qv
  | otherwise = error "The quantum vector is not normalized."

-- | 'amplitude' returns the __probability amplitude__ associated with a
--   given unit vector in the quantum vector.
--
--   __Parameters:__
--
--   - @Map a PA@: a quantum vector represented as a map
--     of basis vectors and probability amplitudes
--   - 'a': the basis vector for which to retrieve the amplitude
--
--   __Returns:__
--
--   - 'PA': the probability amplitude associated with the basis vector,
--     or 0 if not found
amplitude :: Basis a => QV a -> a -> PA
amplitude qvs k = fromMaybe 0 (lookup k qvs)
