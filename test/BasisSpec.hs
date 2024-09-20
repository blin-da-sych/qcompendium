{-# OPTIONS_GHC -Wno-orphans #-}

module BasisSpec
  ( spec
  ) where

import           Basis                    (Colour (Blue, Red, Yellow),
                                           Move (Horizontal, Vertical),
                                           amplitude, qInteger, qVector)
import           Data.Bool                (bool)
import           Test.Hspec               (Spec, describe, it, shouldBe)
import           Test.QuickCheck          (Arbitrary, arbitrary, elements,
                                           property)
import           Test.QuickCheck.Gen      (choose)
import           Test.QuickCheck.Property (forAll)

instance Arbitrary Move where
  arbitrary = elements [Vertical, Horizontal]

instance Arbitrary Colour where
  arbitrary = elements [Red, Yellow, Blue]

spec :: Spec
spec = do
  describe "Quantum Vector Construction (qVector)" $ do
    it "constructs a quantum vector for Bool type" $
      property $ \b -> do
        let vector = qVector [(False, 1), (True, 0)]
        amplitude vector b `shouldBe` bool 1 0 b
    it "constructs a quantum vector for Move type" $
      property $ \m -> do
        let vector = qVector [(Vertical, 0), (Horizontal, 1)]
        amplitude vector m `shouldBe` bool 0 1 (m == Horizontal)
    it "constructs a quantum vector for Colour type" $
      property $ \c -> do
        let vector = qVector [(Red, 1), (Yellow, 0), (Blue, 1)]
        amplitude vector c `shouldBe` bool 1 0 (c == Yellow)
    it "constructs a quantum vector for Integer type (qInteger)" $
      forAll (choose (1, 1000)) $ \n -> do
        let vector = qInteger
        amplitude vector n `shouldBe` (1 / fromIntegral n)
  describe "Infinite Quantum Vector (qInteger)" $ do
    it "retrieves amplitude for specific positive integers from qInteger" $
      forAll (choose (1, 1000)) $ \n -> do
        amplitude qInteger n `shouldBe` (1 / fromIntegral n)
  describe "Probability Retrieval (pr)" $ do
    it "retrieves probability amplitude for Bool type" $
      property $ \b -> do
        let vector = qVector [(False, 1), (True, 0)]
        amplitude vector b `shouldBe` bool 1 0 b
    it "retrieves probability amplitude for Move type" $
      property $ \m -> do
        let vector = qVector [(Vertical, 0), (Horizontal, 1)]
        amplitude vector m `shouldBe` bool 0 1 (m == Horizontal)
    it "retrieves probability amplitude for Colour type" $
      property $ \c -> do
        let vector = qVector [(Red, 1), (Yellow, 0), (Blue, 1)]
        amplitude vector c `shouldBe` bool 1 0 (c == Yellow)
    it "retrieves probability amplitude for Integer type (qInteger)" $
      forAll (choose (1, 1000)) $ \n -> do
        amplitude qInteger n `shouldBe` (1 / fromIntegral n)
