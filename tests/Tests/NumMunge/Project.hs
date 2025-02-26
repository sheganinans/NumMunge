{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.NumMunge.Project where

import Clash.Prelude
import qualified Data.List as L
import Data.Word (Word32)
import NumMunge.Project
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

-- Helper for approximate equality
approxEq :: Fixed16 -> Fixed16 -> Bool
approxEq x y = abs (x - y) < 0.01

case_sqrtFixed :: Assertion
case_sqrtFixed = do
  assertEqual "sqrt 0" 0 (sqrtFixed 0)
  assertEqual "sqrt 1" 1 (sqrtFixed 1)
  assertBool "sqrt 4 ≈ 2" (approxEq (sqrtFixed 4) 2)
  assertBool "sqrt 2 ≈ 1.414" (approxEq (sqrtFixed 2) 1.414_213_562)

case_trigFixed :: Assertion
case_trigFixed = do
  assertEqual "cos 0" 1 (cosFixed 0)
  assertBool "cos pi/2 ≈ 0" (approxEq (cosFixed 1.570_796_3) 0)
  assertEqual "sin 0" 0 (sinFixed 0)
  assertBool "sin pi/2 ≈ 1" (approxEq (sinFixed 1.570_796_3) 1)

case_lnFixed :: Assertion
case_lnFixed = do
  assertEqual "ln 1" 0 (lnFixed 1)
  assertEqual "ln 0" 0 (lnFixed 0)
  assertBool "ln 2.718 ≈ 1" (approxEq (lnFixed 2.718_281_828) 1)

case_xoshiro128Plus :: Assertion
case_xoshiro128Plus = do
  let seed = initialState 42
      (result1, state1) = xoshiro128Plus seed
      (result2, _) = xoshiro128Plus state1
  assertBool "Random numbers differ" (result1 /= result2)
  assertBool "Numbers in Word32 range" (result1 > 0 && result1 < maxBound)

case_randomStream :: Assertion
case_randomStream = do
  let input = pure (initialState 42)
      output = sampleN @System 5 (randomStream input)
  assertBool "Random stream values in [0,1]" $
    all (\x -> x >= 0 && x <= 1) output

case_randomPairs :: Assertion
case_randomPairs = do
  let output =
        sampleN @System 5 $
          withClockResetEnable clockGen resetGen enableGen randomPairs
  assertBool "Random pairs in [0,1]" $
    all (\(x, y) -> x >= 0 && x < 1 && y >= 0 && y < 1) output

case_boxMuller :: Assertion
case_boxMuller = do
  let input = pure (0.5, 0.5)
      output = sampleN @System 5 (boxMuller input)
  assertBool "Box-Muller output in reasonable range" $
    all (\(x, y) -> abs x < 10 && abs y < 10) output

case_topEntity :: Assertion
case_topEntity = do
  let clock = clockGen @System
      reset = resetGen
      enable = enableGen
      output = sampleN @System 5 (topEntity clock reset enable)
  assertBool "Top entity produces reasonable output" $
    all (\(x, y) -> abs x < 10 && abs y < 10) output

prop_rngRange :: Property
prop_rngRange = forAll (vector 1000 :: Gen [Word32]) $ \seeds ->
  let circuit =
        withClockResetEnable clockGen resetGen enableGen $
          randomStream . fmap initialState
      values = simulateN @System 1000 circuit (L.map fromIntegral seeds)
   in all (\x -> x >= 0 && x < 1) values

prop_rngDistribution :: Property
prop_rngDistribution = forAll (choose (1, maxBound) :: Gen Word32) $ \seed ->
  let circuit =
        withClockResetEnable clockGen resetGen enableGen $
          randomStream . fmap initialState
      values = L.take 10_000 $ simulateN @System 10_000 circuit (L.repeat seed)
      buckets = countInBuckets values
      expected = fromIntegral (L.length values `div` 10) :: Double
      chiSquare =
        L.sum
          [ fromIntegral ((observed - expected') * (observed - expected')) / expected
            | let expected' = round expected,
              observed <- buckets
          ]
   in chiSquare < 16.919 -- 95% confidence for 9 degrees of freedom
  where
    countInBuckets vs = L.map count [0 .. 9]
      where
        count i = L.length $ filter (\x -> x >= i / 10 && x < (i + 1) / 10) vs

prop_rngPeriod :: Property
prop_rngPeriod = forAll (choose (1, maxBound) :: Gen Word32) $ \seed ->
  let circuit =
        withClockResetEnable clockGen resetGen enableGen $
          randomStream . fmap initialState
      values = L.take 1000 $ simulateN @System 1000 circuit (L.repeat seed)
   in L.length (L.nub values) > 900

tests :: TestTree
tests = $(testGroupGenerator)
