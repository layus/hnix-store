{-|
Description : Utilities for packing stuff
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Store.Remote.Binary where

import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSL

putInt :: Integral a => a -> Put
putInt = putWord64le . fromIntegral

getInt :: Integral a => Get a
getInt = fromIntegral <$> getWord64le

-- length prefixed string packing with padding to 8 bytes
putByteStringLen :: BSL.ByteString -> Put
putByteStringLen x = do
  putInt len
  putLazyByteString x
  when (len `mod` 8 /= 0) $
    pad $ 8 - (len `mod` 8)
  where
    len :: Int
    len = fromIntegral $ BSL.length x
    pad count = sequence_ $ replicate count (putWord8 0)

putByteStrings :: Foldable t => t BSL.ByteString -> Put
putByteStrings xs = do
  putInt $ length xs
  mapM_ putByteStringLen xs

getByteStringLen :: Get ByteString
getByteStringLen = do
  len <- getInt
  st <- getLazyByteString len
  when (len `mod` 8 /= 0) $ do
    pads <- unpad $ fromIntegral $ 8 - (len `mod` 8)
    unless (all (==0) pads) $ fail $ "No zeroes" ++ show (st, len, pads)
  return $ BSL.toStrict st
  where unpad x = sequence $ replicate x getWord8

getByteStrings :: Get [ByteString]
getByteStrings = do
  count <- getInt
  res <- sequence $ replicate count getByteStringLen
  return res

