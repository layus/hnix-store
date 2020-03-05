{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.ReadonlyStore where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.HashSet as HS
import           Data.Text.Encoding
import           System.Nix.Hash
import           System.Nix.StorePath

makeStorePath :: forall hashAlgo . (NamedAlgo hashAlgo) => FilePath -> ByteString -> Digest hashAlgo -> StorePathName -> StorePath
makeStorePath fp ty h nm = StorePath storeHash nm fp
  where
    s = BS.intercalate ":"
      [ ty
      , encodeUtf8 $ algoName @hashAlgo
      , encodeUtf8 $ encodeBase16 h
      , encodeUtf8 $ T.pack fp
      , encodeUtf8 $ unStorePathName nm
      ]
    storeHash = hash s

makeTextPath :: FilePath -> StorePathName -> Digest 'SHA256 -> StorePathSet -> StorePath
makeTextPath fp nm h refs = makeStorePath fp ty h nm
  where
    ty = BS.intercalate ":" ("text" : map storePathToRawFilePath (HS.toList refs))

makeFixedOutputPath :: (ValidAlgo hashAlgo, NamedAlgo hashAlgo) => FilePath -> Bool -> Digest hashAlgo -> StorePathName -> StorePath storeDir
makeFixedOutputPath fp recursive h nm =
  makeStorePath fp ty h' nm
  where
    (ty, h') =
      if recursive
      then ("source", h)
      else ("output:out", hash ("fixed:out:" <> encodeUtf8 (encodeBase16 h) <> ":"))

computeStorePathForText :: FilePath -> StorePathName -> ByteString -> StorePathSet -> StorePath
computeStorePathForText fp nm s refs = makeTextPath fp nm (hash s) refs
