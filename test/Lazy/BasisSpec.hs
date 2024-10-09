{-# OPTIONS_GHC -Wno-orphans #-}

module Lazy.BasisSpec
  ( spec
  ) where

import           Control.Exception (evaluate)
import           Data.Bool         (bool)
import           Lazy.Basis        (Colour (Blue, Red, Yellow),
                                    Move (Horizontal, Vertical), amplitude,
                                    qInteger, qVector')
import           Test.Hspec        (Spec, describe, errorCall, it, shouldBe,
                                    shouldThrow)
import           Test.QuickCheck   (Arbitrary, arbitrary, choose, elements,
                                    forAll, property)

instance Arbitrary Move where
  arbitrary = elements [Vertical, Horizontal]

instance Arbitrary Colour where
  arbitrary = elements [Red, Yellow, Blue]

spec :: Spec
spec = do
  describe "Quantum Vector Construction (qVector')" $ do
    it "constructs a quantum vector for the Bool type" $
      property $ \b -> do
        let vector = qVector' [(False, 1), (True, 0)]
        amplitude vector b `shouldBe` bool 1 0 b
    it "constructs a quantum vector for the Move type" $
      property $ \m -> do
        let vector = qVector' [(Vertical, 0), (Horizontal, 1)]
        amplitude vector m `shouldBe` bool 0 1 (m == Horizontal)
    it "constructs a quantum vector for the Colour type" $
      property $ \c -> do
        let vector = qVector' [(Red, 0.5), (Yellow, 0.7071067812), (Blue, 0.5)]
        amplitude vector c `shouldBe` bool 0.5 0.7071067812 (c == Yellow)
    it "constructs a quantum vector for the Integer type (qInteger)" $
      forAll (choose (100, 1000)) $ \n -> do
        let vector = qInteger
        amplitude vector n `shouldBe` (1 / sqrt (2 ^ n))
    it
      "throws an error when constructing a non-normalized quantum vector for the Bool type" $ do
      evaluate (qVector' [(False, 0.6), (True, 0.4)]) `shouldThrow`
        errorCall "The quantum vector is not normalized."
  describe "Infinite Quantum Vector (qInteger)" $ do
    it "retrieves the amplitude for a specific positive integer from qInteger" $
      forAll (choose (100, 1000)) $ \n -> do
        amplitude qInteger n `shouldBe` (1 / sqrt (2 ^ n))
  describe "Probability Retrieval (amplitude)" $ do
    it "retrieves the probability amplitude for the Bool type" $
      property $ \b -> do
        let vector = qVector' [(False, 1), (True, 0)]
        amplitude vector b `shouldBe` bool 1 0 b
    it "retrieves the probability amplitude for the Move type" $
      property $ \m -> do
        let vector = qVector' [(Vertical, 0), (Horizontal, 1)]
        amplitude vector m `shouldBe` bool 0 1 (m == Horizontal)
    it "retrieves the probability amplitude for the Colour type" $
      property $ \c -> do
        let vector = qVector' [(Red, 0.5), (Yellow, 1 / sqrt 2), (Blue, 0.5)]
        amplitude vector c `shouldBe` bool 0.5 (1 / sqrt 2) (c == Yellow)
    it "retrieves the probability amplitude for the Integer type (qInteger)" $
      forAll (choose (100, 1000)) $ \n -> do
        amplitude qInteger n `shouldBe` (1 / sqrt (2 ^ n))
