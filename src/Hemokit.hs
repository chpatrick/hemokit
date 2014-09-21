{-# LANGUAGE NamedFieldPuns, TupleSections, DeriveDataTypeable, DeriveGeneric #-}

-- | A library for reading from an Emotic EPOC EEG.
--
-- * Use `getEmotivDevices` to list available EEGs.
--
-- * Use `openEmotivDevice` to open a device for reading.
--
-- * Use `readEmotiv` read from an open device.
--
-- * You will obtain `EmotivPacket`s and `EmotivState`s.
module Hemokit
  ( -- * Opening and reading from EEGs
    _EMOTIV_VENDOR_ID
  , _EMOTIV_PRODUCT_ID
  , EmotivDeviceInfo (..)
  , EmotivRawDevice (..)
  , EmotivDevice (..)
  , getEmotivDevices
  , openEmotivDevice
  , openEmotivDeviceFile
  , openEmotivDeviceHandle
  , readEmotiv
  , EmotivException (..)
  , SerialNumber ()
  , makeSerialNumber
  , makeSerialNumberFromString
  , deviceInfoSerial

  -- EEG models
  , EmotivModel (..)

  -- * EEG data
  , EmotivPacket (..)
  , EmotivState (..)
  , Sensor (..)
  , allSensors

  -- * Encrypted raw data
  , decrypt, serialToAES

  -- * Dealing with (decrypted) raw data
  , EmotivRawData (..)
  , readEmotivRaw
  , makeEmotivRawData
  , parsePacket
  , updateEmotivState

  -- * Internals
  , batteryValue
  , qualitySensorFromByte0

  -- * Interactive use
  , withDataFromLastEEG
  ) where

import           Control.Applicative
import           Control.DeepSeq.Generics
import           Control.Exception
import           Crypto.Cipher.AES
import           Data.Binary.Get (runGet)
import           Data.Binary.Bits.Get
import           Data.Bits
import           Data.Data
import           Data.IORef
import           Data.List
import           Data.Ord (comparing)
import           Data.Traversable (sequenceA)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Data.ByteString as BS (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Lazy (fromStrict)
import           GHC.Generics (Generic)
import qualified System.HIDAPI as HID
import           System.HIDAPI (DeviceInfo (..))
import           System.IO


-- | Whether the EPOC is a consumer or developer model.
--
-- This affects how the EEG data is to be decrypted.
--
-- You can check if you are using the correct model by seeing if the packet
-- `counter` increases from 0 until 128 on subsequent packets.
data EmotivModel = Consumer | Developer deriving (Eq, Ord, Show, Generic)


-- | A valid Emotiv serial number. 16 bytes.
newtype SerialNumber = SerialNumber ByteString deriving (Eq, Ord, Show, Generic)

-- | Checks an Emotiv serial, returning a `SerialNumber` if it's valid.
makeSerialNumber :: ByteString -> Maybe SerialNumber
makeSerialNumber b | BS.length b == 16 = Just $ SerialNumber b
                   | otherwise         = Nothing

-- | Like `makeSerialNumber`, using a `String`.
makeSerialNumberFromString :: String -> Maybe SerialNumber
makeSerialNumberFromString = makeSerialNumber . BS8.pack

-- | Initialize an AES decrypter from a serial number.
serialToAES :: EmotivModel -> SerialNumber -> AES
serialToAES model (SerialNumber num)
  = initAES $ BS.pack key
    where
      serialEnd = map (BS.index num) [ 15, 14, 13, 12 ]
      modelKey = case model of 
        Consumer  -> [ 0x00, 0x54, 0x10, 0x42, 0x00, 0x48, 0x00, 0x50 ]
        Developer -> [ 0x00, 0x48, 0x00, 0x54, 0x10, 0x42, 0x00, 0x50 ]
      -- alternate bytes from the serial and the model-specific key
      key = concat $ transpose [ serialEnd ++ serialEnd, modelKey ]

-- | Takes a 32 bytes encrypted EEG data, returns 32 bytes decrypted EEG data.
decrypt :: AES -> ByteString -> EmotivRawData
decrypt aes encrypted32bytes
  = makeEmotivRawData $ decryptECB aes encrypted32bytes

-- | The sensors of an Emotiv EPOC.
-- Uses the names from the International 10-20 system.
data Sensor
  = F3
  | FC5
  | AF3
  | F7
  | T7
  | P7
  | O1
  | O2
  | P8
  | T8
  | F8
  | AF4
  | FC6
  | F4
  deriving (Eq, Enum, Bounded, Ord, Show, Generic)

-- | Contains all `Sensor`s.
allSensors :: [Sensor]
allSensors = [minBound .. maxBound]

-- | `fromIntegral` shortcut.
int :: (Integral a) => a -> Int
int = fromIntegral


-- TODO this might have to be adjusted
-- | Parses a battery percentage value from a byte.
batteryValue :: Word8 -> Int
batteryValue batteryByte = case batteryByte of
  b | b >= 248 -> 100
  247          -> 99
  246          -> 97
  245          -> 93
  244          -> 89
  243          -> 85
  242          -> 82
  241          -> 77
  240          -> 72
  239          -> 66
  238          -> 62
  237          -> 55
  236          -> 46
  235          -> 32
  234          -> 20
  233          -> 12
  232          -> 6
  231          -> 4
  230          -> 3
  229          -> 2
  228          -> 1
  227          -> 1
  226          -> 1
  _            -> 0


-- | Which sensor's quality is transmitted in the packet
-- (depends on first byte, the packet counter).
qualitySensorFromByte0 :: Word8 -> Maybe Sensor
qualitySensorFromByte0 packetNo = case packetNo of
  0  -> Just F3
  1  -> Just FC5
  2  -> Just AF3
  3  -> Just F7
  4  -> Just T7
  5  -> Just P7
  6  -> Just O1
  7  -> Just O2
  8  -> Just P8
  9  -> Just T8
  10 -> Just F8
  11 -> Just AF4
  12 -> Just FC6
  13 -> Just F4
  14 -> Just F8
  15 -> Just AF4
  64 -> Just F3
  65 -> Just FC5
  66 -> Just AF3
  67 -> Just F7
  68 -> Just T7
  69 -> Just P7
  70 -> Just O1
  71 -> Just O2
  72 -> Just P8
  73 -> Just T8
  74 -> Just F8
  75 -> Just AF4
  76 -> Just FC6
  77 -> Just F4
  78 -> Just F8
  79 -> Just AF4
  80 -> Just FC6
  _  -> Nothing
  -- TODO check why FC6 is the only one that appears 3 times.



-- | Contains the data of a single packet sent from the device.
-- Accumulated data (the current state) is available in `EmotivState`.
data EmotivPacket = EmotivPacket
  { packetCounter :: Int                 -- ^ counts up from 0 to 127 (128 Hz)
  , packetBattery :: Maybe Int           -- ^ the current battery percentage
  , packetGyroX   :: Int                 -- ^ turning "left" gives positive numbers
  , packetGyroY   :: Int                 -- ^ turning "down" gives positive numbers
  , packetSensors :: Vector Int          -- ^ EEG sensor values
  , packetQuality :: Maybe (Sensor, Int) -- ^ EEG sensor-to-skin connectivity
  } deriving (Eq, Ord, Show, Generic)


-- | Contains the "current state" of the EEG, cumulateively updated by
-- incoming `EmotivPacket`s.
data EmotivState = EmotivState
  { counter   :: Int        -- ^ counts up from 0 to 127 (128 Hz)
  , battery   :: Int        -- ^ the current battery percentage
  , gyroX     :: Int        -- ^ turning "left" gives positive numbers
  , gyroY     :: Int        -- ^ turning "down" gives positive numbers
  , sensors   :: Vector Int -- ^ EEG sensor values
  , qualities :: Vector Int -- ^ EEG sensor-to-skin connectivity
  } deriving (Eq, Ord, Show, Generic)


-- | Wraps (unencrypted) Emotiv raw data. Ensures that it is 32 bytes.
newtype EmotivRawData = EmotivRawData
  { emotivRawDataBytes :: ByteString
  } deriving (Eq, Ord, Show, Generic)


-- | Treat a `ByteString` as Emotiv raw data.
-- Errors if the input is non 32 bytes.
makeEmotivRawData :: ByteString -> EmotivRawData
makeEmotivRawData bytes
  | BS.length bytes /= 32 = error "Emotiv raw data must be 32 bytes"
  | otherwise             = EmotivRawData bytes

-- I improved the gyro like this:
-- https://github.com/openyou/emokit/commit/b023a3c195410147dae44a3ce3a6d72f7c16e441

-- | Parses an `EmotivPacket` from raw bytes.
parsePacket :: EmotivRawData -> EmotivPacket
parsePacket (EmotivRawData bytes32)
  = runGet packetParser $ fromStrict bytes32
    where 
      sevenLevels = sequenceA . replicate 7 $ word16be 14
      packetParser = runBitGet . block $ buildPacket
        <$> bool          -- counter or battery indicator
        <*> word8 7       -- counter or battery value
        <*> sevenLevels   -- first seven signal levels
        <*  bool          -- mystery bit
        <*> word16be 14   -- signal quality
        <*  word16be 13   -- more mystery bits
        <*> sevenLevels   -- next seven signal levels
        <*> word16be 8    -- 8 high bits of gyro X
        <*> word16be 8    -- 8 high bits of gyro Y
        <*> word16be 4    -- 4 low bits of gyro X
        <*> word16be 4    -- 4 low bits of gyro Y
      buildPacket isBattery batteryCounter s1 q s2 hx hy lx ly
        = EmotivPacket
          { packetCounter = if isBattery then 128 else int batteryCounter
          , packetBattery = if isBattery then Just (batteryValue batteryCounter) else Nothing
          , packetGyroX = int (hx `shiftL` 4 .|. lx) - 1652 -- TODO check this hardcoding
          , packetGyroY = int (hy `shiftL` 4 .|. ly) - 1681
          , packetSensors = V.fromListN 14 $ map int (s1 ++ s2)
          , packetQuality = ( , int q ) <$> qualitySensorFromByte0 batteryCounter
          }

-- | The USB vendor ID of the Emotiv EPOC.
_EMOTIV_VENDOR_ID :: HID.VendorID
_EMOTIV_VENDOR_ID = 8609

-- | The USB product ID of the Emotiv EPOC.
_EMOTIV_PRODUCT_ID :: HID.ProductID
_EMOTIV_PRODUCT_ID = 1


-- | Emotiv related errors.
data EmotivException
  = InvalidSerialNumber HID.SerialNumber -- ^ Serial does not have right format.
  | CouldNotReadSerial HID.DevicePath    -- ^ We could not read the serial from the device.
  | OtherEmotivException String
  deriving (Data, Typeable, Generic)

instance Exception EmotivException

instance Show EmotivException where
  show (InvalidSerialNumber sn)   = "Emotiv ERROR: the device serial number " ++ sn ++ " does not look valid"
  show (CouldNotReadSerial path)  = "Emotiv ERROR: could not read serial number of device " ++ path ++ ". Maybe you are not running as root?"
  show (OtherEmotivException err) = "Emotiv ERROR: " ++ err


-- | Identifies an Emotiv device.
data EmotivDeviceInfo = EmotivDeviceInfo
  { hidapiDeviceInfo :: DeviceInfo -- ^ The hidapi device info.
  } deriving (Show, Generic)


-- | An "open" data source to read bytes from.
data EmotivRawDevice
  = HidapiDevice
      { hidapiDevice :: HID.Device -- ^ The open hidapi device.
      }
  | HandleDevice
      { handleDevice :: Handle -- ^ A conventional `Handle`, e.g. an open file.
      } deriving (Generic)


-- | Identifies an open Emotiv device.
-- Also contains the cumulative `EmotivState` of the EEG.
data EmotivDevice = EmotivDevice
  { rawDevice    :: EmotivRawDevice           -- ^ Where we get our data from, some form of "open handle".
  , deviceAES    :: AES                       -- ^ AES decrypter for the device.
  , stateRef     :: IORef (Maybe EmotivState) -- ^ The EEG's cumulative state.
  } deriving (Generic)


-- | Conveniently expose the serial number of a device.
deviceInfoSerial :: EmotivDeviceInfo -> Maybe SerialNumber
deviceInfoSerial = (>>= makeSerialNumberFromString) . serialNumber . hidapiDeviceInfo


-- | Lists all EPOC devices, ordered by interface number.
-- If you do not actively choose amongst them, the last one is usually the one
-- you want (especially if only 1 EEG is connected).
getEmotivDevices :: IO [EmotivDeviceInfo]
getEmotivDevices = map EmotivDeviceInfo
                 . sortBy (comparing interfaceNumber)
                 <$> HID.enumerate (Just _EMOTIV_VENDOR_ID) (Just _EMOTIV_PRODUCT_ID)


-- | Opens a given Emotiv device.
-- Returns an `EmotivDevice` to read from with `readEmotiv`.
openEmotivDevice :: EmotivModel -> EmotivDeviceInfo -> IO EmotivDevice
openEmotivDevice model EmotivDeviceInfo{ hidapiDeviceInfo } = case hidapiDeviceInfo of
  DeviceInfo{ serialNumber = Nothing, path } -> throwIO $ CouldNotReadSerial path
  DeviceInfo{ serialNumber = Just sn } ->
    case makeSerialNumberFromString sn of
      Nothing -> throwIO $ InvalidSerialNumber sn
      Just s  -> do hidDev <- HID.openDeviceInfo hidapiDeviceInfo
                    stateRef <- newIORef Nothing
                    return $ EmotivDevice
                      { rawDevice   = HidapiDevice hidDev
                      , deviceAES   = serialToAES model s
                      }


-- | Creates an `EmotivDevice` device from a path, e.g. a device like
-- @/dev/hidraw1@ or a normal file containing dumped binary data.
openEmotivDeviceFile :: EmotivModel -> SerialNumber -> String -> IO EmotivDevice
openEmotivDeviceFile model sn path = do
  h <- openFile path ReadMode
  openEmotivDeviceHandle model sn h


-- | Creates an `EmotivDevice` device from an open file handle.
openEmotivDeviceHandle :: EmotivModel -> SerialNumber -> Handle -> IO EmotivDevice
openEmotivDeviceHandle model sn h = do
  stateRef <- newIORef Nothing
  return $ EmotivDevice
    { rawDevice   = HandleDevice h
    , deviceAES   = serialToAES model sn
    , stateRef    = stateRef
    }


-- | Reads one 32 byte packet from the device and decrypts it to raw data.
--
-- Returns Nothing on end of input (or if there are < 32 bytes before it).
--
-- Note that if the EEG is (turned) off, this function block until
-- it is turned on again.
readEmotivRaw :: EmotivDevice -> IO (Maybe EmotivRawData)
readEmotivRaw EmotivDevice{ rawDevice, deviceAES } = do

  d32 <- case rawDevice of HidapiDevice d -> HID.read d 32
                           HandleDevice d -> BS.hGet d 32

  -- If we get less than the requested 32 bytes, we're at EOF.
  return $ if BS.length d32 < 32
             then Nothing
             else Just $ decrypt deviceAES d32


-- | Given a device and a Emotiv raw data, parses the raw data into an
-- `EmotivPacket` and updates the cumulative `EmotivState` that we maintain
-- for that device.
--
-- Care should be taken that raw data is fed into this function in the right
-- order (e.g. respecting the EEG's increasing sequence numbers and quality
-- updates).
--
-- This function is only neededif you want to obtain both raw data and
-- parsed packages.
-- If you are not interested in raw data, use `readEmotiv` instead.
--
-- Returns both the packet read from the device and the updated state.
updateEmotivState :: EmotivDevice -> EmotivRawData -> IO (EmotivState, EmotivPacket)
updateEmotivState EmotivDevice{ stateRef } rawData = do

  let p = parsePacket rawData

  -- Update accumulative state

  lastState <- readIORef stateRef

  let lastBattery   = maybe 0 battery lastState
      lastQualities = maybe (V.replicate (length allSensors) 0) qualities lastState

      newState = EmotivState
        { counter   = packetCounter p
        , battery   = maybe lastBattery id (packetBattery p)
        , gyroX     = packetGyroX p
        , gyroY     = packetGyroY p
        , sensors   = packetSensors p
        -- We can't get around an O(n) qualities vector copy here
        -- if we want `Eq EmotivState`. It's small enough anyway.
        , qualities = lastQualities `deepseq` case packetQuality p of
                        Nothing          -> lastQualities
                        Just (sensor, l) -> lastQualities V.// [(fromEnum sensor, l)]
        }

  writeIORef stateRef (Just newState)

  return (newState, p)


-- | Reads one 32 byte packet from the device, parses the raw bytes into an
-- `EmotivPacket` and updates the cumulative `EmotivState` that we maintain
-- for that device.
--
-- Returns both the packet read from the device and the updated state.
--
-- Returns Nothing on end of input (or if there are < 32 bytes before it).
--
-- Note that if the EEG is (turned) off, this function block until
-- it is turned on again.
readEmotiv :: EmotivDevice -> IO (Maybe (EmotivState, EmotivPacket))
readEmotiv device = do m'raw <- readEmotivRaw device
                       case m'raw of
                         Nothing  -> return Nothing
                         Just raw -> Just <$> updateEmotivState device raw


-- | Opens and reads from the last available device, giving all data from it
-- to the given function. Stops if end of input is reached.
--
-- Intended for use with ghci.
--
-- Examples:
--
-- >withDataFromLastEEG Consumer print
-- >withDataFromLastEEG Consumer (print . packetQuality . snd)
-- >withDataFromLastEEG Consumer (putStrLn . unwords . map show . V.toList . qualities . fst)
withDataFromLastEEG :: EmotivModel -> ((EmotivState, EmotivPacket) -> IO ()) -> IO ()
withDataFromLastEEG model f = do
  devices <- getEmotivDevices
  device <- case devices of
    [] -> error "No devices found."
    _  -> openEmotivDevice model (last devices)
  let run = do m'd <- readEmotiv device
               case m'd of Nothing -> return () -- terminate
                           Just d  -> f d >> run
  run


-- * NFData instances
instance NFData EmotivDeviceInfo where rnf = genericRnf
instance NFData EmotivException where rnf = genericRnf
instance NFData EmotivPacket where rnf = genericRnf
instance NFData EmotivRawData where rnf = genericRnf
instance NFData EmotivState where rnf = genericRnf
instance NFData Sensor where rnf = genericRnf
