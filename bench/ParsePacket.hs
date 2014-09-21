{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main (main) where

import           Control.Applicative
import           Criterion.Main
import qualified Data.ByteString as BS
import           Data.Bits
import           Data.Word
import qualified Data.Vector.Unboxed as V

import Hemokit

-- | Describes the indices of bits to make up a certain value.
newtype BitMask = BitMask [Word8] deriving (Eq, Ord, Show)

int :: (Integral a, Num b) => a -> b
int = fromIntegral

-- | Describes which bits in a raw data packet make up the given sensor.
getSensorMask :: Sensor -> BitMask
getSensorMask s = BitMask $ case s of
  F3  -> [10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7]
  FC5 -> [28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23, 8, 9]
  AF3 -> [46, 47, 32, 33, 34, 35, 36, 37, 38, 39, 24, 25, 26, 27]
  F7  -> [48, 49, 50, 51, 52, 53, 54, 55, 40, 41, 42, 43, 44, 45]
  T7  -> [66, 67, 68, 69, 70, 71, 56, 57, 58, 59, 60, 61, 62, 63]
  P7  -> [84, 85, 86, 87, 72, 73, 74, 75, 76, 77, 78, 79, 64, 65]
  O1  -> [102, 103, 88, 89, 90, 91, 92, 93, 94, 95, 80, 81, 82, 83]
  O2  -> [140, 141, 142, 143, 128, 129, 130, 131, 132, 133, 134, 135, 120, 121]
  P8  -> [158, 159, 144, 145, 146, 147, 148, 149, 150, 151, 136, 137, 138, 139]
  T8  -> [160, 161, 162, 163, 164, 165, 166, 167, 152, 153, 154, 155, 156, 157]
  F8  -> [178, 179, 180, 181, 182, 183, 168, 169, 170, 171, 172, 173, 174, 175]
  AF4 -> [196, 197, 198, 199, 184, 185, 186, 187, 188, 189, 190, 191, 176, 177]
  FC6 -> [214, 215, 200, 201, 202, 203, 204, 205, 206, 207, 192, 193, 194, 195]
  F4  -> [216, 217, 218, 219, 220, 221, 222, 223, 208, 209, 210, 211, 212, 213]

-- | Describes which bits in a raw data packat make up a sensor quality value.
qualityMask :: BitMask
qualityMask = BitMask [99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112]


-- | Extracts the sensor value for the given sensor from Emotiv raw data.
getLevel :: EmotivRawData -> BitMask -> Int
getLevel (EmotivRawData bytes32) (BitMask sensorBits) = foldr f 0 sensorBits
  where
    f :: Word8 -> Int -> Int
    f bitNo level = (level `shiftL` 1) .|. int (bitAt b o)
      where
        b = (bitNo `shiftR` 3) + 1 :: Word8 -- div by 8 to get byte number, skip first byte (counter)
        o = bitNo .&. 7            :: Word8 -- mod by 8 to get bit offset

    bitAt :: Word8 -> Word8 -> Word8
    bitAt byte bitOffset = ((bytes32 `BS.index` int byte) `shiftR` int bitOffset) .&. 1

-- | Parses an `EmotivPacket` from raw bytes.
parsePacketBitmask :: EmotivRawData -> EmotivPacket
parsePacketBitmask raw@(EmotivRawData bytes32) = EmotivPacket
  { packetCounter = if is128c then 128                       else fromIntegral byte0
  , packetBattery = if is128c then Just (batteryValue byte0) else Nothing
  , packetGyroX   = ((int (byte 29) `shiftL` 4) .|. int (byte 31 `shiftR` 4)) - 1652 -- TODO check this hardcoding
  , packetGyroY   = ((int (byte 30) `shiftL` 4) .|. int (byte 31   .&. 0x0F)) - 1681
  , packetSensors = V.fromList [ getLevel raw (getSensorMask s) | s <- allSensors ]
  , packetQuality = (, getLevel raw qualityMask) <$> qualitySensorFromByte0 byte0
  }
  where
    byte0  = byte 0
    byte n = bytes32 `BS.index` n
    is128c = byte0 .&. 128 /= 0 -- Is it the packet which would be sequence no 128?
                                -- If so, then byte0 is the battery value.

main :: IO ()
main = defaultMain
  [ bench "parsePacketBitGet" $ nf parsePacket rawData
  , bench "parsePacketBitMask" $ nf parsePacketBitmask rawData
  ]
  where
    rawData = makeEmotivRawData "P}::\199\183\221\193|!\250h\205\NUL\NUL\NUL\STX\ENQX\r\162E|\218)\ETB\155\US\224gi9"
