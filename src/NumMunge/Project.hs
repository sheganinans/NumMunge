{-# OPTIONS_GHC -Wno-orphans #-}

module NumMunge.Project where

import Clash.Prelude
import qualified Data.List as L
import Data.Word (Word32)
import NumMunge.GaussianNoise (Fixed16, generateNoise)
import Protocols hiding (circuit)
import Protocols.Axi4.Common
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.Stream
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Text.Printf (printf)

type ConfAW = 'Axi4WriteAddressConfig 'True 'True 2 2 'True 'True 'True 'True 'True 'True

type ConfW = 'Axi4WriteDataConfig 'True 2

type ConfB = 'Axi4WriteResponseConfig 'True 2

type ConfAR = 'Axi4ReadAddressConfig 'True 'True 2 2 'True 'True 'True 'True 'True 'True

type ConfR = 'Axi4ReadDataConfig 'True 2

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
