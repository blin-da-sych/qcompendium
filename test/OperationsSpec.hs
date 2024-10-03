module OperationsSpec
  ( spec
  ) where

import           Basis             (Rotation (Clockwise, CtrClockwise),
                                    amplitude, qVector')
import           Control.Exception (evaluate)
import           Data.Bool         (bool)
import           Data.Complex      (magnitude)
import           Operations        (cnot, hadamard, qApp, qApp', qLift, qNot,
                                    qop, toffoli)
import           Test.Hspec        (Spec, describe, errorCall, it, shouldBe,
                                    shouldNotBe, shouldThrow)
import           Test.QuickCheck   (property)

spec :: Spec
spec = do
  describe "Quantum NOT Operation (qNot)" $ do
    it "flip the amplitudes of a quantum vector of type Bool" $
      property $ \b -> do
        let vector = qVector' [(False, 1), (True, 0)]
        let result = qNot vector
        amplitude result b `shouldBe` bool 0 1 b
    it "should throw an error for non-normalized vector" $ do
      evaluate (qNot (qVector' [(False, 0.6), (True, 0.6)])) `shouldThrow`
        errorCall "The quantum vector is not normalized."
  describe "Hadamard Operation (hadamard)" $ do
    it "create a superposition from the |0⟩ state" $
      let vector = qVector' [(False, 1), (True, 0)]
          result = hadamard vector
          expected = 1 / sqrt 2
       in magnitude (amplitude result False) `shouldBe` magnitude expected
    it "create a superposition from the |1⟩ state" $
      let vector = qVector' [(False, 0), (True, 1)]
          result = hadamard vector
          expected = 1 / sqrt 2
       in magnitude (amplitude result False) `shouldBe` magnitude expected
    it "should throw an error for a non-normalized input vector" $ do
      evaluate (hadamard (qVector' [(False, 0.6), (True, 0.6)])) `shouldThrow`
        errorCall "The quantum vector is not normalized."
  describe "Quantum Operator Application (qApp') - normalized version" $ do
    it "apply a quantum operator on a quantum vector" $ do
      let vector = qVector' [(CtrClockwise, sqrt 8 / 3), (Clockwise, 1 / 3)]
          operator = qop [((CtrClockwise, False), 1), ((Clockwise, True), 0)]
          result = qApp' operator vector
      amplitude result False `shouldBe` 1
      amplitude result True `shouldBe` 0
    it "apply a non-trivial quantum operator" $ do
      let vector = qVector' [(False, 1), (True, 0)]
          operator =
            qop
              [ ((False, False), 0.5)
              , ((False, True), 0.5)
              , ((True, False), 0.5)
              , ((True, True), 0.5)
              ]
          result = qApp' operator vector
      amplitude result False `shouldBe` 1 / sqrt 2
      amplitude result True `shouldBe` 1 / sqrt 2
  describe "Quantum Operator Construction (qop)" $ do
    it "construct a quantum operator from a list of basis-pair amplitudes" $
      let operator = qop [((False, False), 0), ((True, True), 1)]
       in amplitude (qApp' operator (qVector' [(True, 1), (False, 0)])) False `shouldBe`
          0
  describe "Quantum Operator Construction (qop) - Unitarity" $ do
    it "quantum vector operation is unitary" $ do
      let vector = qVector' [(False, 1), (True, 0)]
          operator = qop [((False, False), 1), ((True, True), 1)]
          result = (qApp' operator . qApp' operator) vector
      vector `shouldBe` result
    it "quantum vector operation is non-unitary" $ do
      let vector = qVector' [(False, 0), (True, 1)]
          operator = qop [((False, False), 0.8)]
          result = (qApp operator . qApp operator) vector
      vector `shouldNotBe` result
  describe "Controlled NOT Operation (cnot)" $ do
    it "flip the second qubit if the first qubit is True" $
      -- Initial state |00⟩ corresponds to ((False, False), 1)
     do
      let vector = qVector' [((False, False), 1), ((True, False), 0)] -- Initial state |00⟩
      let result = qApp cnot vector
      amplitude result (False, False) `shouldBe` 1                    -- |00⟩ remains |00⟩
      amplitude result (True, False) `shouldBe` 0                     -- |10⟩ remains |10⟩
      amplitude result (True, True) `shouldBe` 0                      -- |11⟩ remains |11⟩
      amplitude result (False, True) `shouldBe` 0                     -- No flip if control is |0⟩
  describe "Toffoli Operation (toffoli)" $ do
    it "flip the target qubit if both control qubits are True" $
      -- Initial state |000⟩ corresponds to ((False, False), False)
     do
      let vector =
            qVector'
              [ (((False, False), False), 1) -- |000⟩
              , (((False, True), False), 0)  -- |010⟩
              , (((True, False), False), 0)  -- |100⟩
              , (((True, True), False), 0)   -- |110⟩
              ]
      let result = qApp toffoli vector
      amplitude result ((False, False), False) `shouldBe` 1 -- |000⟩ remains unchanged
      amplitude result ((True, True), True) `shouldBe` 0    -- |110⟩ is flipped to |111⟩
    it "leave the target qubit unchanged if at least one control qubit is False" $
      -- Initial state |100⟩ corresponds to ((True, False), False)
     do
      let vector =
            qVector'
              [ (((True, False), False), 1)  -- |100⟩
              , (((True, True), False), 0)   -- |110⟩
              , (((False, False), False), 0) -- |000⟩
              , (((False, True), False), 0)  -- |010⟩
              ]
          result = qApp toffoli vector
      amplitude result ((True, False), False) `shouldBe` 1  -- |100⟩ remains unchanged
      amplitude result ((True, True), False) `shouldBe` 0   -- |110⟩ remains unchanged
      amplitude result ((False, False), False) `shouldBe` 0 -- |000⟩ remains unchanged
      amplitude result ((False, True), False) `shouldBe` 0  -- |010⟩ remains unchanged
  describe "qLift" $ do
    it "lift the classical NOT function for Bool to a quantum operator" $ do
      let notOp = qLift not
          vector = qVector' [(False, 1), (True, 0)] -- |0⟩
          result = qApp' notOp vector
      amplitude result False `shouldBe` 0           -- |0⟩ flips to |1⟩
      amplitude result True `shouldBe` 1            -- |1⟩ from |0⟩
    it "lift an identity function on Bool to a quantum operator" $ do
      let idOp = qLift id
          vector = qVector' [(False, 1), (True, 0)] -- |0⟩
          result = qApp' idOp vector
      amplitude result False `shouldBe` 1           -- |0⟩ remains |0⟩
      amplitude result True `shouldBe` 0            -- |1⟩ remains |1⟩
    it "lift a swap function for (Bool, Bool) basis" $ do
      let swap (a, b) = (b, a)
          swapOp = qLift swap
          vector = qVector' [((True, False), 1), ((False, True), 0)] -- |10⟩
          result = qApp' swapOp vector
      amplitude result (False, True) `shouldBe` 1                    -- |10⟩ is swapped to |01⟩
      amplitude result (True, False) `shouldBe` 0                    -- |10⟩ is swapped to |01⟩
    it "lift a constant function to a quantum operator" $ do
      let constOp = qLift (const False)             -- Always returns False
          vector = qVector' [(True, 1), (False, 0)] -- |1⟩ state
          result = qApp' constOp vector
      amplitude result False `shouldBe` 1           -- |1⟩ becomes |0⟩
      amplitude result True `shouldBe` 0            -- No |1⟩ after applying const False
