{-# OPTIONS_GHC -Wno-orphans #-}

module NumMunge.GaussianNoise where

import Clash.Prelude
import qualified Data.List as L
import Data.Word (Word32)
import Text.Printf (printf)

type Fixed16 = SFixed 16 16

type Seed = (Word32, Word32, Word32, Word32)

generateNoise :: (HiddenClockResetEnable dom) => Signal dom (Fixed16, Fixed16)
generateNoise = boxMuller randomStream

sqrtFixed :: Fixed16 -> Fixed16
sqrtFixed x = if x == 0 then 0 else go 5 (if x < 1 then 0.5 else x / 1.414213562)
  where
    go :: Int -> Fixed16 -> Fixed16
    go 0 y = y
    go i y = go (i - 1) ((y + (x / y)) / 2)

cosFixed :: Fixed16 -> Fixed16
cosFixed x = 1 - (x2 / 2) + (x4 / 24) - (x6 / 720)
  where
    x2 = x * x
    x4 = x2 * x2
    x6 = x4 * x2

sinFixed :: Fixed16 -> Fixed16
sinFixed x = x - (x3 / 6) + (x5 / 120) - (x7 / 5040)
  where
    x2 = x * x
    x3 = x2 * x
    x4 = x2 * x2
    x5 = x4 * x
    x6 = x3 * x3
    x7 = x6 * x

lnFixed :: Fixed16 -> Fixed16
lnFixed x
  | x <= 0 || x == 1 = 0
  | x < 1 = -(go (1 / x))
  | otherwise = go x
  where
    go :: Fixed16 -> Fixed16
    go x' = 2 * (t + t3 / 3 + t5 / 5 + t7 / 7)
      where
        t = (x' - 1) / (x' + 1)
        t2 = t * t
        t3 = t2 * t
        t4 = t2 * t2
        t5 = t4 * t
        t6 = t3 * t3
        t7 = t6 * t

boxMuller ::
  (HiddenClockResetEnable dom) =>
  Signal dom Fixed16 ->
  Signal dom (Fixed16, Fixed16)
boxMuller u = bundle (z0, z1)
  where
    r = fmap (* 0.5) $ sqrtFixed <$> fmap ((* (-2)) . lnFixed) u
    theta = fmap (* 6.28318530718) u
    z0 = (*) <$> r <*> fmap cosFixed theta
    z1 = (*) <$> r <*> fmap sinFixed theta

type State = (Word32, Word32, Word32, Word32)

initialState :: Word32 -> Seed
initialState seed = (seed, seed `xor` 0x01234567, seed `xor` 0x89ABCDEF, seed `xor` 0xDEADBEEF)

randomStream :: (HiddenClockResetEnable dom) => Signal dom Fixed16
randomStream = fmap toFixed stateSignal
  where
    initState = initialState 0xC0FFEE42
    stateSignal = register initState $ fmap (snd . xoshiro128Plus) stateSignal
    toFixed state =
      let (randWord, _) = xoshiro128Plus state
          scaledVal = fromIntegral ((randWord `shiftR` 16) .&. 0x7FFF) / 32768
       in scaledVal

xoshiro128Plus :: State -> (Word32, State)
xoshiro128Plus (s0, s1, s2, s3) =
  let result = s0 + s3
      s1' = s1 `xor` (s0 `shiftL` 9)
      s2' = s2 `xor` (s1 `shiftL` 13)
      s3' = s3 `xor` (s2 `rotateL` 7)
      s0' = s0 `xor` (s3 `shiftL` 4)
      nextState = (s1', s2', s3', s0')
   in (result, nextState)

{-

  Debugging functions

-}

testFullCircuit :: [(Fixed16, Fixed16)]
testFullCircuit = simulateN @System 100 circuit (L.repeat ())
  where
    circuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom (Fixed16, Fixed16)
    circuit _ = exposeClockResetEnable generateNoise clockGen resetGen enableGen

debugTest :: IO ()
debugTest = do
  putStrLn "Circuit simulation results:"
  mapM_ printSample $ L.drop 10 $ L.zip [(1 :: Int) ..] testFullCircuit
  where
    printSample (i, (x, y)) =
      printf
        "Sample %3d: (%8.4f, %8.4f)\n"
        i
        (realToFrac x :: Double)
        (realToFrac y :: Double)

debugStateTransition :: IO ()
debugStateTransition = do
  putStrLn "State Transition Debugging:"
  let initState' = initialState 0xDEADBEEF
      states = L.take 10 $ L.iterate (snd . xoshiro128Plus) initState'
      values = L.map toFixed states
  mapM_ printTransition $ L.zip [1 ..] (L.zip states values)
  where
    toFixed state =
      let (randWord, _) = xoshiro128Plus state
          scaledVal = fromIntegral ((randWord `shiftR` 16) .&. 0x7FFF) / 32768
       in scaledVal

    printTransition :: (Int, (Seed, Fixed16)) -> IO ()
    printTransition (i, (state, val)) =
      printf
        "Step %3d: State=%s Value=%.6f\n"
        i
        (show state)
        (realToFrac val :: Double)

debugDistribution :: IO ()
debugDistribution = do
  putStrLn "Distribution of random values:"
  let values = L.take 1000 $ simulateN @System 1000 circuit (L.repeat ())
      ranges = [(i / 10, (i + 1) / 10) | i <- [0 .. 9]]
      buckets = countBuckets values ranges
  mapM_ (printBucket ranges) $ L.zip [0 ..] buckets
  where
    circuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom Fixed16
    circuit _ = withClockResetEnable clockGen resetGen enableGen randomStream

    countBuckets :: [Fixed16] -> [(Fixed16, Fixed16)] -> [Int]
    countBuckets vs = L.map count
      where
        count (l, h) = L.length $ filter (\x -> x >= l && x < h) vs

    printBucket :: [(Fixed16, Fixed16)] -> (Int, Int) -> IO ()
    printBucket rs (idx, count) =
      printf
        "[%.1f-%.1f): %d\n"
        (realToFrac l :: Double)
        (realToFrac h :: Double)
        count
      where
        (l, h) = rs L.!! idx

debugBoxMuller :: IO ()
debugBoxMuller = do
  putStrLn "Box-Muller transform test:"
  let circuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom (Fixed16, Fixed16)
      circuit _ = boxMuller randomStream
      pairs = L.take 20 $ simulateN @System 20 circuit (L.repeat ())
  mapM_ printPair $ L.zip [1 :: Int ..] pairs
  where
    printPair (i, (x, y)) =
      printf
        "Pair %2d: (%8.4f, %8.4f)\n"
        i
        (realToFrac x :: Double)
        (realToFrac y :: Double)

debugRange :: IO ()
debugRange = do
  putStrLn "Range verification:"
  let values = L.take 100 $ simulateN @System 100 circuit (L.repeat ())
  printf "Min value: %.6f\n" (realToFrac (L.minimum values) :: Double)
  printf "Max value: %.6f\n" (realToFrac (L.maximum values) :: Double)
  where
    circuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom Fixed16
    circuit _ = withClockResetEnable clockGen resetGen enableGen randomStream
