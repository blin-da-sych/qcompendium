{-# LANGUAGE ScopedTypeVariables #-}

module PairsSpec
  ( spec
  ) where

import           Basis           (Basis (basis), amplitude, qVector)
import           Pairs           ((&*))
import           Test.Hspec      (Spec, describe, it, shouldBe)
import           Test.QuickCheck (property)

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
      let qa = qVector [(False, 0.5), (True, 0.5)]
          qb = qVector [(False, 0.6), (True, 0.4)]
          result = qa &* qb
       in do amplitude result (False, False) `shouldBe` 0.5 * 0.6
             amplitude result (False, True) `shouldBe` 0.5 * 0.4
             amplitude result (True, False) `shouldBe` 0.5 * 0.6
             amplitude result (True, True) `shouldBe` 0.5 * 0.4
  describe "Quantum vector construction with (&*)" $ do
    it "constructs a quantum vector for Bool ⊗ Bool" $
      property $ \(b1 :: Bool, b2 :: Bool) -> do
        let qa = qVector [(False, 1), (True, 0)]
            qb = qVector [(False, 0), (True, 1)]
            result = qa &* qb
        amplitude result (b1, b2) `shouldBe` amplitude qa b1 * amplitude qb b2
