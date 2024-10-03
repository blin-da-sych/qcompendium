{-|
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

-- | 'qNotOp' defines the quantum NOT operation (Pauli-X gate) as a quantum operator
--   that acts on a single qubit (basis type 'Bool'). It flips the basis states
--   'True' and 'False'.
--
--   __Returns:__
--
--   - @Qop Bool Bool@: A quantum operator that implements the NOT operation on
--     single qubits.
qNotOp :: Qop Bool Bool
qNotOp = qop [((False, True), 1), ((True, False), 1)]

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

-- | 'qLift' lifts a classical function into the quantum operator domain by creating a
--   quantum operator ('Qop') that represents the function's mapping from basis vectors
--   of type 'a' to type 'b'.
--
--   __Parameters:__
--
--   - A function @f@ of type @(a -> b)@, which maps basis vectors of type 'a'
--     to basis vectors of type 'b'.
--
--   __Returns:__
--
--   - A quantum operator @Qop a b@, represented as a list of pairs where each pair
--     consists of a basis vector 'a' and its corresponding image under the function 'f'.
--     The probability amplitude for each mapping is set to 1, indicating a deterministic
--     transformation from 'a' to 'b'.
qLift :: (Basis a, Basis b) => (a -> b) -> Qop a b
qLift f = qop [((a, f a), 1) | a <- basis]

-- | 'cop' constructs a quantum operator that combines two systems,
--   filtering the basis states based on a given enabling condition.
--   It effectively applies the quantum operator 'u' to states where
--   the enabling condition is met, while leaving other states unchanged.
--
--   __Parameters:__
--
--   - @enable@: A predicate function that takes an element of type 'a'
--     and returns a 'Bool'. This function determines whether the quantum
--     operator 'u' should be applied to a particular basis state 'a'.
--   - @Qop b b@: A quantum operator that acts on the basis type 'b'.
--     It defines how the basis states of type 'b' are transformed when
--     the corresponding state of type 'a' is enabled.
--
--   __Returns:__
--
--   - @Qop (a, b) (a, b)@: A new quantum operator that acts on pairs
--     of basis states (of types 'a' and 'b'). For pairs where the
--     enabling condition is not met, the state remains unchanged.
--     For pairs where the enabling condition is met, the quantum
--     operator 'u' is applied to the second component of the pair.
cop :: (Basis a, Basis b) => (a -> Bool) -> Qop b b -> Qop (a, b) (a, b)
cop enable (Qop u) =
  qop
    ([(((a, b), (a, b)), 1) | (a, b) <- basis, not (enable a)] ++
     [ (((a, b1), (a, b2)), amplitude u (b1, b2))
     | a <- basis
     , enable a
     , b1 <- basis
     , b2 <- basis
     ])

-- | 'cnot' defines the quantum controlled NOT operation (CNOT gate) as a quantum operator
--   that acts on a two-qubit system (basis type @(Bool, Bool)@). The first qubit acts as
--   the control qubit, while the second qubit is the target qubit.
--
--   __Returns:__
--
--   - @Qop (Bool, Bool) (Bool, Bool)@: A quantum operator that represents the CNOT operation,
--     which conditionally flips the target qubit based on the state of the control qubit.
cnot :: Qop (Bool, Bool) (Bool, Bool)
cnot = cop id qNotOp

-- | 'toffoli' defines the quantum Toffoli gate (controlled-controlled NOT) as a quantum operator
--   that acts on a three-qubit system, represented as a pair of qubits (control qubits) and
--   a target qubit (basis type @((Bool, Bool), Bool)@).
--
--   __Returns:__
--
--   - @Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)@: A quantum operator that represents
--     the Toffoli operation, which conditionally flips the target qubit based on the
--     states of the two control qubits.
toffoli :: Qop ((Bool, Bool), Bool) ((Bool, Bool), Bool)
toffoli = cop (uncurry (&&)) qNotOp
