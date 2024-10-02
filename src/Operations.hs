{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Operations
Description : Quantum operations on quantum vectors and quantum operators

This module defines various quantum operations on quantum vectors ('QV') and
quantum operators ('Qop'). It provides utility functions to construct quantum
operators from a list of basis vector pairs and their associated probability
amplitudes, apply quantum gates such as the NOT and Hadamard gates, and
perform matrix-vector multiplication using quantum operators.
-}
module Operations where

import           Basis    (Basis, PA, QV, amplitude, basis, qVector, qVector')
import           Data.Map (Map)
import           Pairs    ()

-- | 'Qop' is a __quantum operator__ type alias, which represents a linear operator
--   on a quantum vector space. It is defined as a 'Map' from pairs of basis vectors
--   (of types 'a' and 'b') to probability amplitudes ('PA').
newtype Qop a b =
  Qop (Map (a, b) PA)
  deriving (Show)

-- | 'qop' constructs a quantum operator ('Qop') from a list of basis vector
--   pairs and their associated probability amplitudes.
--
--   __Parameters:__
--
--   - A list of pairs @[((a, b), PA)]@, where 'a' and 'b' are basis vectors
--     and 'PA' is the probability amplitude (a complex number)
--
--   __Returns:__
--
--   - @Qop a b@: a quantum operator, represented as a Map from pairs of basis vectors
--     to probability amplitudes
qop :: (Basis a, Basis b) => [((a, b), PA)] -> Qop a b
qop = Qop . qVector

-- | 'qNot' applies the quantum __NOT__ operation (or Pauli-X gate) on a quantum
--   vector of type @QV Bool@. It flips the probability amplitudes associated
--   with 'True' and 'False' basis states.
--
--   __Parameters:__
--
--   - @QV Bool@: a quantum vector defined on the basis set of 'Bool'
--
--   __Returns:__
--
--   - @QV Bool@: a new quantum vector, where the amplitudes of 'True' and 'False'
--     have been swapped
qNot :: QV Bool -> QV Bool
qNot v = qVector' [(False, amplitude v True), (True, amplitude v False)]

-- | 'hadamard' applies the __Hadamard operation__ (Hadamard gate) on a quantum
--   vector of type @QV Bool@. It creates a normalized superposition of the 'False'
--   and 'True' basis states, ensuring that the resulting quantum vector has a total
--   probability of 1.
--
--   __Parameters:__
--
--   - @QV Bool@: a quantum vector defined on the basis set of 'Bool'
--
--   __Returns:__
--
--   - @QV Bool@: a new normalized quantum vector representing the result of applying
--     the Hadamard gate, which creates an equal superposition of 'False' and 'True'
--     with a preserved norm.
hadamard :: QV Bool -> QV Bool
hadamard v =
  let a = amplitude v False
      b = amplitude v True
      hFactor = 1 / sqrt 2
   in qVector' [(False, hFactor * (a + b)), (True, hFactor * (a - b))]

-- | 'qApp' applies a quantum operator ('Qop') on a quantum vector ('QV').
--   It performs matrix-vector multiplication, where the quantum operator
--   acts on the quantum vector. __The resulting vector is denormalized.__
--
--   __Parameters:__
--
--   - @Qop a b@: a quantum operator acting from basis type 'a' to basis type 'b'
--   - @QV a@: a quantum vector defined on the basis set of type 'a'
--
--   __Returns:__
--
--   - @QV b@: a new quantum vector defined on the basis set of type 'b',
--     representing the result of applying the quantum operator on the input vector
qApp :: (Basis b, Basis a) => Qop a b -> QV a -> QV b
qApp (Qop m) v =
  let bF b = sum [amplitude m (a, b) * amplitude v a | a <- basis]
   in qVector [(b, bF b) | b <- basis]

-- | 'qApp'' applies a quantum operator ('Qop') on a quantum vector ('QV').
--   It performs matrix-vector multiplication, where the quantum operator
--   acts on the quantum vector and ensures __the resulting vector is normalized__.
--   This function computes the norm of the resulting vector and adjusts the
--   amplitudes accordingly to guarantee a total norm of 1.
--
--   __Parameters:__
--
--   - @Qop a b@: a quantum operator acting from basis type 'a' to basis type 'b'
--   - @QV a@: a quantum vector defined on the basis set of type 'a'
--
--   __Returns:__
--
--   - @QV b@: a new quantum vector defined on the basis set of type 'b',
--     representing the result of applying the quantum operator on the input vector,
--     normalized to ensure valid probabilities and a total norm of 1.
qApp' :: (Basis b, Basis a) => Qop a b -> QV a -> QV b
qApp' (Qop m) v =
  let bF b = sum [amplitude m (a, b) * amplitude v a | a <- basis]
      unnormalized = [(b, bF b) | b <- basis]
      norm =
        sqrt . sum $
        [amplitude (qVector unnormalized) b ^ (2 :: Int) | b <- basis]
   in qVector' [(b, amp / norm) | (b, amp) <- unnormalized]
