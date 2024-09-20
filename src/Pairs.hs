{-# OPTIONS_GHC -Wno-orphans #-}

module Pairs where

import           Basis (Basis (basis), QV, amplitude, qVector)

-- | Instance of 'Basis' for pairs of types ('a', 'b') where both 'a' and 'b'
--   have a basis. The basis of the pair is formed by the Cartesian product
--   of the two bases.
instance (Basis a, Basis b) => Basis (a, b) where
  basis = [(a, b) | a <- basis, b <- basis]

-- | '&*' - tensor product of two quantum vectors. The resulting quantum vector represents
--   the tensor product of the input quantum vectors.
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a, b)
qa &* qb = qVector [((a, b), amplitude qa a * amplitude qb b) | (a, b) <- basis]
