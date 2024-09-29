{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PairsSpec
  ( spec
  ) where

import           BasisStrict       (Basis (basis), Colour (..), Move (..),
                                    amplitude, qVector)
import           Control.Exception (evaluate)
import           Pairs             ((&*))
import           Test.Hspec        (Spec, describe, errorCall, it, shouldBe,
                                    shouldThrow)
import           Test.QuickCheck   (Arbitrary (..), elements, property)

instance Arbitrary Move where
  arbitrary = elements [Vertical, Horizontal]

instance Arbitrary Colour where
  arbitrary = elements [Red, Yellow, Blue]

spec :: Spec
spec = do
  describe "Basis instance for (a, b)" $ do
    it "forms Cartesian product of basis for Bool" $
      basis `shouldBe`
      [(False, False), (False, True), (True, False), (True, True)]
  describe "Tensor product (&*) for quantum vectors" $ do
    it "computes tensor product of two quantum vectors (Bool, Bool)" $
      let qa = qVector [(False, 1), (True, 0)]
          qb = qVector [(False, 0), (True, 1)]
          result = qa &* qb
       in do amplitude result (False, False) `shouldBe` 0
             amplitude result (False, True) `shouldBe` 1
             amplitude result (True, False) `shouldBe` 0
             amplitude result (True, True) `shouldBe` 0
    it "computes tensor product of quantum vectors with different amplitudes" $
      let qa = qVector [(False, 1 / sqrt 2), (True, 1 / sqrt 2)]
          qb = qVector [(False, sqrt 3 / 2), (True, 1 / 2)]
          result = qa &* qb
       in do amplitude result (False, False) `shouldBe` (1 / sqrt 2) *
               (sqrt 3 / 2)
             amplitude result (False, True) `shouldBe` 1 / sqrt 2 * 0.5
             amplitude result (True, False) `shouldBe` (1 / sqrt 2) *
               (sqrt 3 / 2)
             amplitude result (True, True) `shouldBe` 1 / sqrt 2 * 0.5
    it "computes tensor product of quantum vectors for Move and Colour types" $
      let qa = qVector [(Vertical, sqrt 8 / 3), (Horizontal, 1 / 3)]
          qb = qVector [(Red, sqrt 15 / 4), (Blue, 1 / 4)]
          result = qa &* qb
       in do amplitude result (Vertical, Red) `shouldBe` (sqrt 8 / 3) *
               (sqrt 15 / 4)
             amplitude result (Vertical, Blue) `shouldBe` (sqrt 8 / 3) * (1 / 4)
             amplitude result (Horizontal, Red) `shouldBe` (1 / 3) *
               (sqrt 15 / 4)
             amplitude result (Horizontal, Blue) `shouldBe` (1 / 3) * (1 / 4)
  describe "Quantum vector construction with (&*)" $ do
    it "constructs a quantum vector for Bool ⊗ Bool" $
      property $ \(b1 :: Bool, b2 :: Bool) -> do
        let qa = qVector [(False, 1), (True, 0)]
            qb = qVector [(False, 0), (True, 1)]
            result = qa &* qb
        amplitude result (b1, b2) `shouldBe` amplitude qa b1 * amplitude qb b2
    it "constructs a quantum vector for Move ⊗ Colour" $
      property $ \(m :: Move, c :: Colour) -> do
        let qa = qVector [(Vertical, 1 / 2), (Horizontal, sqrt 3 / 2)]
            qb = qVector [(Red, 1 / 3), (Yellow, sqrt 8 / 3)]
            result = qa &* qb
        amplitude result (m, c) `shouldBe` amplitude qa m * amplitude qb c
    it
      "should throw an error while computes tensor product of unnormalized quantum vectors" $ do
      let qa = qVector [(False, 0.5), (True, 0.5)]
          qb = qVector [(False, 0.6), (True, 0.4)]
      evaluate (qa &* qb) `shouldThrow`
        errorCall "The quantum vector is not normalized."
