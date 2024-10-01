{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Pairs
Description : Tensor product and basis for paired quantum systems

This module provides the '(&*)' operator to compute the tensor product of two quantum
vectors, combining the states of two quantum systems into a single state. The tensor
product is fundamental in quantum mechanics, enabling the representation of entangled
systems, where the state of one system cannot be described independently of the other.
-}
module Pairs where

import           Basis (Basis (basis), QV, amplitude, qVector')

instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]

-- | The @&*@ operation represents the __tensor product__ of two quantum vectors.
--   The resulting quantum vector represents the tensor product of the input quantum vectors
--   and can be interpreted as a representation of quantum entanglement between the two systems.
--
--   This operation combines the states of two quantum systems into a single state that
--   can exhibit entangled properties. This means the state of one system cannot be
--   described independently of the state of the other, even when separated by large distances.
--
--   __Parameters:__
--
--   - @QV a@: The first quantum vector representing the state of the first system.
--   - @QV b@: The second quantum vector representing the state of the second system.
--
--   __Returns:__
--
--   - @QV (a, b)@: A quantum vector representing the tensor product of @qa@ and @qb@,
--     embodying the combined state of the two systems that may be entangled.
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
qa &* qb =
  qVector' [((a, b), amplitude qa a * amplitude qb b) | (a, b) <- basis]
