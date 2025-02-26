{-# OPTIONS_GHC -Wno-orphans #-}

module NumMunge.Project where

import Clash.Prelude
import qualified Data.List as L
import Data.Word (Word32)
import Text.Printf (printf)

type Fixed16 = SFixed 16 16

type Seed = (Word32, Word32, Word32, Word32)

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "gaussianNoise",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Fixed16, Fixed16)
topEntity = exposeClockResetEnable generateNoise

generateNoise :: (HiddenClockResetEnable dom) => Signal dom (Fixed16, Fixed16)
generateNoise = boxMuller randomPairs

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
  Signal dom (Fixed16, Fixed16) ->
  Signal dom (Fixed16, Fixed16)
boxMuller uniform = bundle (z0, z1)
  where
    (u1, u2) = unbundle uniform
    r = fmap (* 0.5) $ sqrtFixed <$> fmap ((* (-2)) . lnFixed) u1
    theta = fmap (* 6.28318530718) u2
    z0 = (*) <$> r <*> fmap cosFixed theta
    z1 = (*) <$> r <*> fmap sinFixed theta

type State = (Word32, Word32, Word32, Word32)

xoshiro128Plus :: State -> (Word32, State)
xoshiro128Plus (s0, s1, s2, s3) =
  let result = s0 + s3
      s1' = s1 `xor` (s0 `shiftL` 9)
      s2' = s2 `xor` (s1 `shiftL` 13)
      s3' = s3 `xor` (s2 `rotateL` 7)
      s0' = s0 `xor` (s3 `shiftL` 4)
      nextState = (s1', s2', s3', s0')
   in (result, nextState)

initialState :: Word32 -> Seed
initialState seed = (seed, seed `xor` 0x01234567, seed `xor` 0x89ABCDEF, seed `xor` 0xDEADBEEF)

randomPairs :: (HiddenClockResetEnable dom) => Signal dom (Fixed16, Fixed16)
randomPairs = bundle (r1, r2)
  where
    r1 = randomStream $ pure $ initialState 0xDEADBEEF
    r2 = randomStream $ pure $ initialState 0x12345678

randomStream :: (HiddenClockResetEnable dom) => Signal dom Seed -> Signal dom Fixed16
randomStream _ = fmap toFixed stateSignal
  where
    initState = initialState 0xC0FFEE42
    stateSignal = register initState $ fmap (snd . xoshiro128Plus) stateSignal
    toFixed state =
      let (randWord, _) = xoshiro128Plus state
          scaledVal = fromIntegral ((randWord `shiftR` 16) .&. 0x7FFF) / 32768
       in scaledVal

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

debugState :: IO ()
debugState = do
  putStrLn "RNG State transitions:"
  let testInputs :: [Word32]
      testInputs = L.take 10 (L.replicate 5 0xDEADBEEF L.++ L.replicate 5 0x12345678)

      initState' = initialState 0xDEADBEEF

      transitions :: [Seed]
      transitions = L.scanl step initState' testInputs

      outputs :: [Fixed16]
      outputs = L.zipWith (curry (genOutput . xoshiro128Plus . fst)) transitions (L.tail transitions)

  mapM_ printTransition $ L.zip3 [1 ..] transitions outputs
  where
    step :: Seed -> Word32 -> Seed
    step curSeed _ = snd $ xoshiro128Plus curSeed

    genOutput :: (Word32, Seed) -> Fixed16
    genOutput (randWord, _) =
      fromRational $ toRational randWord / toRational (maxBound :: Word32)

    printTransition :: (Int, Seed, Fixed16) -> IO ()
    printTransition (i, state, val) =
      printf
        "Step %3d: State=%s Value=%f\n"
        i
        (show state)
        (realToFrac val :: Double)

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

testRNG :: IO ()
testRNG = do
  putStrLn "Testing RNG output:"
  let initState' = initialState 0xDEADBEEF
      outputs :: [(Word32, State)]
      outputs = L.take 10 $ L.iterate (xoshiro128Plus . snd) (xoshiro128Plus initState')

  mapM_ (\(i, (val, _)) -> printf "Output %3d: %08x\n" (i :: Int) val) $
    L.zip [1 ..] outputs

debugValues :: IO ()
debugValues = do
  putStrLn "Random value distribution check:"
  let values = simulateN @System 20 rngCircuit (L.repeat 0)

  mapM_
    ( \(i, v) ->
        printf
          "Value %2d: %f\n"
          (i :: Int)
          (realToFrac v :: Double)
    )
    $ L.zip [1 ..] values
  where
    rngCircuit ::
      (HiddenClockResetEnable dom) =>
      Signal dom Word32 ->
      Signal dom Fixed16
    rngCircuit _ =
      let stateSignal =
            register initState $
              fmap (snd . xoshiro128Plus) stateSignal
          initState = initialState 0xDEADBEEF
       in fmap genOutput stateSignal
      where
        genOutput s =
          let (randWord, _) = xoshiro128Plus s
           in fromRational $
                toRational randWord / toRational (maxBound :: Word32)

testSeed :: [(Fixed16, Fixed16)]
testSeed = simulateN @System 100 circuit (L.repeat ())
  where
    circuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom (Fixed16, Fixed16)
    circuit _ = exposeClockResetEnable generateNoise clockGen resetGen enableGen

debugBoxMuller :: IO ()
debugBoxMuller = do
  putStrLn "Box-Muller transform test:"
  let circuit :: (HiddenClockResetEnable dom) => Signal dom () -> Signal dom (Fixed16, Fixed16)
      circuit _ = boxMuller $ bundle (r1, r2)
        where
          r1 = register 0.1 $ randomStream s1
          r2 = register 0.1 $ randomStream s2
          s1 = pure $ initialState 0xDEADBEEF
          s2 = pure $ initialState 0x12345678
      pairs = simulateN @System 10 circuit (L.repeat ())
  mapM_
    ( \(i, (x, y)) ->
        printf
          "Pair %2d: (%8.4f, %8.4f)\n"
          i
          (realToFrac x :: Double)
          (realToFrac y :: Double)
    )
    $ L.zip [1 :: Int ..] pairs

debugDistribution :: IO ()
debugDistribution = do
  putStrLn "Distribution of random values:"
  let values = L.take 1000 $ simulateN @System 1000 (circuit systemClockGen systemResetGen enableGen) (L.repeat 0)
      ranges = [(i / 10, (i + 1) / 10) | i <- [0 .. 9]]
      buckets = countBuckets values ranges
  mapM_ (printBucket ranges) $ L.zip [0 ..] buckets
  where
    circuit ::
      Clock System ->
      Reset System ->
      Enable System ->
      Signal System Word32 ->
      Signal System Fixed16
    circuit clk rst en input =
      withClockResetEnable clk rst en $
        randomStream (fmap initialState input)

    countBuckets :: [Fixed16] -> [(Fixed16, Fixed16)] -> [Int]
    countBuckets vs = L.map count
      where
        count (l, h) = L.length $ filter (\x -> x >= l && x < h) vs

    printBucket :: [(Fixed16, Fixed16)] -> (Int, Int) -> IO ()
    printBucket rs (idx, count) =
      printf "[%.1f-%.1f): %d\n" (realToFrac l :: Double) (realToFrac h :: Double) count
      where
        (l, h) = rs L.!! idx

debugRange :: IO ()
debugRange = do
  putStrLn "Range verification:"
  let values = L.take 100 $ simulateN @System 100 (circuit systemClockGen systemResetGen enableGen) (L.repeat 0)
  printf "Min value: %.6f\n" (realToFrac (L.minimum values) :: Double)
  printf "Max value: %.6f\n" (realToFrac (L.maximum values) :: Double)
  where
    circuit clk rst en input =
      withClockResetEnable clk rst en $
        randomStream (fmap initialState input)
