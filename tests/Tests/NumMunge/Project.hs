{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.NumMunge.Project where

import Clash.Prelude
import qualified Data.List as L
import NumMunge.GaussianNoise
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

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
  let output = sampleN @System 500 $ withClockResetEnable clockGen resetGen enableGen randomStream
  assertBool "Random stream values in [0,1)" $
    all (\x -> x >= 0 && x < 1) output

case_boxMuller :: Assertion
case_boxMuller = do
  let input = pure 0.5
      output = sampleN @System 500 (boxMuller input)
  assertBool "Box-Muller output in reasonable range" $
    all (\(x, y) -> abs x < 10 && abs y < 10) output

case_topEntity :: Assertion
case_topEntity = do
  let output = sampleN @System 500 $ withClockResetEnable clockGen resetGen enableGen generateNoise
  assertBool "Top entity produces reasonable output" $
    all (\(x, y) -> abs x < 10 && abs y < 10) output

prop_rngRange :: Property
prop_rngRange =
  property $
    let circuit =
          withClockResetEnable clockGen resetGen enableGen randomStream
        values = sampleN @System 1000 circuit
     in conjoin $ L.map (\x -> x >= 0 Test.Tasty.QuickCheck..&&. x < 1) values

prop_rngDistribution :: Property
prop_rngDistribution =
  property $
    let circuit = withClockResetEnable clockGen resetGen enableGen randomStream
        values = sampleN @System 10_000 circuit
        buckets = countInBuckets values
        expected = fromIntegral (L.length values `div` 10) :: Double
        chiSquare =
          sum
            [ fromIntegral ((observed - expected') * (observed - expected')) / expected
              | let expected' = round expected,
                observed <- buckets
            ]
     in counterexample ("Chi-square value: " L.++ show chiSquare) $
          chiSquare < 16.919 -- 95% confidence for 9 degrees of freedom
  where
    countInBuckets vs = L.map count [0 .. 9]
      where
        count i = L.length $ L.filter (\x -> x >= i / 10 && x < (i + 1) / 10) vs

prop_rngPeriod :: Property
prop_rngPeriod =
  property $
    let circuit = withClockResetEnable clockGen resetGen enableGen randomStream
        values = sampleN @System 1000 circuit
        uniqueCount = L.length (L.nub values)
     in counterexample
          ("Unique values: " L.++ show uniqueCount L.++ " out of 1000")
          $ uniqueCount > 900

tests :: TestTree
tests = $(testGroupGenerator)
