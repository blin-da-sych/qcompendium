{-# OPTIONS_GHC -Wno-orphans #-}

module Pairs where

import           BasisStrict (Basis (basis), QV, amplitude, qVector)

instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]

-- | '&*' - **tensor product** of two quantum vectors. The resulting quantum vector
--   represents the tensor product of the input quantum vectors and can be interpreted
--   as a representation of quantum entanglement between the two systems.
--
--   This operation combines the states of two quantum systems into a single state
--   that can exhibit entangled properties, meaning the state of one system cannot
--   be described independently of the state of the other, even when separated by
--   large distances.
--
--   ==== Parameters:
--   - @QV a@: the first quantum vector representing the state of the first system
--   - @QV b@: the second quantum vector representing the state of the second system
--
--   ==== Returns:
--   - @QV (a, b)@: a quantum vector representing the tensor product of 'qa' and 'qb',
--     embodying the combined state of the two systems that may be entangled.
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
qa &* qb = qVector [((a, b), amplitude qa a * amplitude qb b) | (a, b) <- basis]
