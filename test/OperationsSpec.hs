module OperationsSpec
  ( spec
  ) where

import           Basis             (Rotation (Clockwise, CtrClockwise),
                                    amplitude, qVector')
import           Control.Exception (evaluate)
import           Data.Bool         (bool)
import           Data.Complex      (magnitude)
import           Operations        (hadamard, qApp, qApp', qNot, qop)
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
