module Measurement
  ( normalize
  , norm
  , (*>)
  ) where

import           Basis        (Basis, PA, QV)

import           Data.Bool    (bool)
import           Data.Complex (Complex ((:+)), magnitude)
import           Data.Map     (elems)
import           Prelude      hiding ((*>))

-- | 'normalize' takes a quantum vector and returns a normalized version of it.
--   If the vector is already normalized, it scales the amplitudes accordingly.
--   If the vector's norm is zero, it throws an error.
--
--   __Parameters:__
--
--   - A quantum vector of type @QV a@, where 'a' is the basis vector type.
--
--   __Returns:__
--
--   - A normalized quantum vector of the same type.
--
--   __Throws:__
--
--   - An 'error' with the message *"NaN encountered in normalization."*
--     if the vector's norm is zero.
normalize :: Basis a => QV a -> QV a
normalize v =
  let n = norm v
   in bool
        ((1 / n :+ 0) *> v)
        (error "NaN encountered in normalization.")
        (n == 0)

-- | 'norm' computes the norm of a given quantum vector by summing the squares
--   of the magnitudes of its probability amplitudes. This is useful for
--   determining whether a vector is normalized.
--
--   __Parameters:__
--
--   - A quantum vector of type @QV a@.
--
--   __Returns:__
--
--   - A 'Double' representing the norm of the quantum vector.
norm :: Basis a => QV a -> Double
norm v =
  let probs = map (square . magnitude) (elems v)
      square = (^ (2 :: Int))
   in sqrt (sum probs)

-- | The operator '*>' scales a quantum vector by a given probability amplitude.
--   It multiplies each amplitude in the quantum vector by the given complex number.
--
--   __Parameters:__
--
--   - A complex number representing the probability amplitude.
--   - A quantum vector of type @QV a@.
--
--   __Returns:__
--
--   - A scaled quantum vector.
(*>) :: Basis a => PA -> QV a -> QV a
c *> v = fmap (c *) v
