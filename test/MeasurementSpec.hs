module MeasurementSpec
  ( spec
  ) where

import           Basis             (qVector, qVector')
import           Control.Exception (evaluate)
import           Data.Complex      (Complex ((:+)))
import           Data.Map          (fromList)
import           Measurement       (norm, normalize, (*>))
import           Prelude           hiding ((*>))
import           Test.Hspec        (Spec, describe, errorCall, it, shouldBe,
                                    shouldNotBe, shouldThrow)

square :: Complex Double -> Complex Double
square = (^ (2 :: Int))

spec :: Spec
spec = do
  describe "Norm Calculation (norm)" $ do
    it "calculates the norm of a normalized quantum vector" $ do
      let vector = qVector' [((False, False), 1), ((True, True), 0)]
      norm vector `shouldBe` 1.0
    it "calculates the norm of a non-normalized quantum vector" $ do
      let vector = qVector [((False, False), 1 / sqrt 2), ((True, True), 0.8)]
      norm vector `shouldNotBe` 1.0
  describe "Vector Normalization (normalize)" $ do
    it "normalizes a quantum vector" $ do
      let vector = qVector [((False, False), 0.6), ((True, True), 0.8)]
          expected =
            qVector
              [ ((False, False), 0.6 / sqrt (square 0.6 + square 0.8))
              , ((True, True), 0.8 / sqrt (square 0.6 + square 0.8))
              ]
      normalize vector `shouldBe` expected
    it "normalizes a quantum vector with one of the amplitudes equal to 0" $ do
      let vector = qVector [((False, False), 0.6), ((True, True), 0)]
          expected =
            qVector
              [ ((False, False), 0.6 / sqrt (square 0.6 + square 0))
              , ((True, True), 0 / sqrt (square 0.6 + square 0))
              ]
      normalize vector `shouldBe` expected
    it "throws an error when normalizing a zero vector" $ do
      let zeroVector = qVector [((False, False), 0), ((True, True), 0)]
      evaluate (normalize zeroVector) `shouldThrow`
        errorCall "NaN encountered in normalization."
  describe "Quantum Vector Scaling ((*>) operator)" $ do
    it "scales a quantum vector by a given complex number" $ do
      let vector = qVector' [((False, False), 1), ((True, True), 0)]
          scalar = 0.5 :+ 0
          expected = qVector [((False, False), 0.5), ((True, True), 0)]
      (scalar *> vector) `shouldBe` expected
    it "scales a quantum vector by zero" $ do
      let vector = fromList [((False, False), 1), ((True, True), 0)]
          scalar = 0 :+ 0
          expected = fromList [((False, False), 0), ((True, True), 0)]
      (scalar *> vector) `shouldBe` expected
    it "returns the same vector when scaled by one" $ do
      let vector = fromList [((False, False), 1), ((True, True), 0)]
          scalar = 1 :+ 0
      (scalar *> vector) `shouldBe` vector
