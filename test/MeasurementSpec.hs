module MeasurementSpec
  ( spec
  ) where

import           Basis             (amplitude, qVector, qVector')
import           Control.Exception (evaluate)
import           Data.Complex      (Complex ((:+)))
import           Data.IORef        (readIORef)
import           Data.Map          (fromList)
import           Measurement       (QR (..), mkQR, norm, normalize, (*>))
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
      scalar *> vector `shouldBe` expected
    it "scales a quantum vector by zero" $ do
      let vector = fromList [((False, False), 1), ((True, True), 0)]
          scalar = 0 :+ 0
          expected = fromList [((False, False), 0), ((True, True), 0)]
      scalar *> vector `shouldBe` expected
    it "returns the same vector when scaled by one" $ do
      let vector = fromList [((False, False), 1), ((True, True), 0)]
          scalar = 1 :+ 0
      scalar *> vector `shouldBe` vector
  describe "Quantum Register Creation (mkQR)" $
    it "creates a quantum register holding the correct quantum vector" $ do
      let vector = qVector [(True, 0.8 :+ 0), (False, 0.6 :+ 0)]
      qr <- mkQR vector
      QR ref <- return qr
      storedV <- readIORef ref
      amplitude storedV True `shouldBe` 0.8 :+ 0
      amplitude storedV False `shouldBe` 0.6 :+ 0
  -- describe "Quantum Register Observation (observeR)" $
  --   it "collapses the quantum register to a single state after observation" $ do
  --     let vector = qVector [(True, 0.9 :+ 0), (False, 0.1 :+ 0)]
  --     qr <- mkQR vector
  --     result <- observeR qr
  --     QR ref <- return qr
  --     storedV <- readIORef ref
  --     amplitude storedV result `shouldBe` 1 :+ 0 -- Collapsed to a single state
  -- describe "Quantum Vector Observation (observeV)" $
  --   it "returns a basis element based on the probability distribution" $ do
  --     let vector = qVector [(True, 0.5 :+ 0), (False, 0.5 :+ 0)]
  --     result <- observeV vector
  --     result `shouldSatisfy` (`elem` [True, False])
  -- describe "Left Component Observation (observeLeft)" $
  --   it
  --     "collapses the left component of a bipartite quantum register after observation" $ do
  --     let vector =
  --           qVector
  --             [ ((True, True), 0.7 :+ 0)
  --             , ((True, False), 0.3 :+ 0)
  --             , ((False, True), 0.0 :+ 0)
  --             ]
  --     qr <- mkQR vector
  --     result <- observeLeft qr
  --     result `shouldBe` True
  --     QR ref <- return qr
  --     storedV <- readIORef ref
  --     amplitude storedV (True, True) `shouldBe` 0.7 :+ 0
  --     amplitude storedV (True, False) `shouldBe` 0.3 :+ 0
