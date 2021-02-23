{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module System.Nix.ReadonlyStore where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.HashSet as HS
import           Data.Text.Encoding
import           System.Nix.Hash
import           System.Nix.Nar
import           System.Nix.StorePath
import           Control.Monad.State.Strict

makeStorePath :: forall hashAlgo . (NamedAlgo hashAlgo)
  => FilePath
  -> ByteString
  -> Digest hashAlgo
  -> StorePathName
  -> StorePath
makeStorePath fp ty h nm = StorePath storeHash nm fp
  where
    s = BS.intercalate ":"
      [ ty
      , encodeUtf8 $ algoName @hashAlgo
      , encodeUtf8 $ encodeInBase Base16 h
      , encodeUtf8 $ T.pack fp
      , encodeUtf8 $ unStorePathName nm
      ]
    storeHash = hash s

makeTextPath :: FilePath -> StorePathName -> Digest 'SHA256 -> StorePathSet -> StorePath
makeTextPath fp nm h refs = makeStorePath fp ty h nm
  where
    ty = BS.intercalate ":" ("text" : map storePathToRawFilePath (HS.toList refs))

makeFixedOutputPath :: forall hashAlgo . (ValidAlgo hashAlgo, NamedAlgo hashAlgo)
  => FilePath
  -> Bool
  -> Digest hashAlgo
  -> StorePathName
  -> StorePath
makeFixedOutputPath fp recursive h nm =
  if recursive && (algoName @hashAlgo) == "sha256"
  then makeStorePath fp "source"     h  nm
  else makeStorePath fp "output:out" h' nm
 where
  h' = hash @'SHA256 $ "fixed:out:" <> encodeUtf8 (algoName @hashAlgo) <> (if recursive then ":r:" else ":") <> encodeUtf8 (encodeInBase Base16 h) <> ":"

computeStorePathForText :: FilePath -> StorePathName -> ByteString -> StorePathSet -> StorePath
computeStorePathForText fp nm s refs = makeTextPath fp nm (hash s) refs

computeStorePathForPath :: forall a. (ValidAlgo a, NamedAlgo a)
           => StorePathName        -- ^ Name part of the newly created `StorePath`
           -> FilePath             -- ^ Local `FilePath` to add
           -> Bool                 -- ^ Add target directory recursively
           -> (FilePath -> Bool)   -- ^ Path filter function
           -> Bool                 -- ^ Only used by local store backend
           -> IO StorePath
computeStorePathForPath name pth recursive _pathFilter _repair = do
  selectedHash <- if recursive then recursiveContentHash else flatContentHash
  pure $ makeFixedOutputPath "/nix/store" recursive selectedHash name
 where
  recursiveContentHash :: IO (Digest a)
  recursiveContentHash = finalize @a <$> execStateT streamNarUpdate (initialize @a)
  streamNarUpdate :: StateT (AlgoCtx a) IO ()
  streamNarUpdate = streamNarIO (modify . flip (update @a)) narEffectsIO pth

  flatContentHash :: IO (Digest a)
  flatContentHash = hashLazy <$> narReadFile narEffectsIO pth
