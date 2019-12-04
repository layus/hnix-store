{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.ReadonlyStore where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import           Data.Text.Encoding
import           System.Nix.Hash
import           System.Nix.Hash as H
import           System.Nix.StoreAPI
import           System.Nix.StorePath

instance StoreAPI ReadonlyStore where
    makeStorePath = makeStorePath

makeStorePath :: forall storeDir hashAlgo . (KnownStoreDir storeDir, NamedAlgo hashAlgo) => ByteString -> Digest hashAlgo -> StorePathName -> StorePath storeDir
makeStorePath ty h nm = StorePath storeHash nm
  where
    s = BS.intercalate ":"
      [ ty
      , encodeUtf8 $ algoName @hashAlgo
      , encodeUtf8 $ encodeBase16 h
      , storeDirVal @storeDir
      , encodeUtf8 $ unStorePathName nm
      ]
    storeHash = hash s


makeTextPath :: (KnownStoreDir storeDir) => StorePathName -> Digest 'SHA256 -> StorePathSet storeDir -> StorePath storeDir
makeTextPath nm h refs = makeStorePath ty h nm
  where
    ty = BS.intercalate ":" ("text" : map storePathToRawFilePath (HS.toList refs))

computeStorePathForText :: (KnownStoreDir storeDir) => StorePathName -> ByteString -> StorePathSet storeDir -> StorePath storeDir
computeStorePathForText nm s refs = makeTextPath nm (hash s) refs


hack :: forall hashAlgo. (NamedAlgo hashAlgo) => RecursiveFlag -> Bool
hack = recursive && (algoName @hashAlgo == "sha256")

makeType :: forall storeDir.(IsString s, KnownStoreDir storeDir)
  => BS.ByteString -> HS.HashSet (StorePath storeDir)
makeType typeName references =
    BS.intercalate ":" ([ typeName ] ++ map storePathToRawFilePath references)

{-
    /* Constructs a unique store path name. */
    Path makeStorePath(const string & type,
        const Hash & hash, const string & name) const;
 
    Path makeOutputPath(const string & id,
        const Hash & hash, const string & name) const;
 
    Path makeFixedOutputPath(bool recursive,
        const Hash & hash, const string & name,
        const PathSet & references = {}) const;
-}
makeFixedOutputPath :: forall storeDir hashAlgo. (NamedAlgo hashAlgo, ValidAlgo hashAlgo, KnownStoreDir storeDir)
    => RecursiveFlag
    -> Digest hashAlgo
    -> StorePathName
    -> HS.HashSet (StorePath storeDir)
    -> StorePath storeDir
makeFixedOutputPath recursive hash name references =
  if hack @hashAlgo recursive
  then makeStorePath (makeType "source" references) hash name
  else
    -- assert HS.null references
    makeStorePath "output:out" hash' name
 where
   hash' = H.hash @'SHA256 $ BS.intercalate (
     "fixed:out:"
     ++ (if recursive then "r:" else "")
     ++ (encodeBase16 hash) 
     ++ ":"
     )

{- 
    Path makeTextPath(const string & name, const Hash & hash,
        const PathSet & references) const;
-}

addToStoreFromDump :: forall hashType storeDir. (NamedAlgo hashType, ValidAlgo hashType, KnownStoreDir storeDir)
    => Put
    -> StorePathName
    -> RecursiveFlag
    -> RepairFlag
    -> StorePath storeDir
addToStoreFromDump dump name recursive repair =
    -- XXX What should we do with the content ?
    dstPath
  where
    h = hash @hashType (runPut dump)
    dstPath = makeFixedOutputPath recursive h name HS.null



addToStore :: forall hashType storeDir. (NamedAlgo hashType, ValidAlgo hashType, KnownStoreDir storeDir)
    => StorePathName
    -> FilePath
    -> RecursiveFlag
    -> FilePathFilter
    -> RepairFlag
    -> IO (StorePath storeDir)
addToStore name srcPath recursive pathFilter repair = do
    dump <- input
    addToStoreFromDump @hashType dump
  where
    input = if recursive
            then putNar <$> localPackNar' narEffectsIO srcPath pathFilter
            else putLazyByteString <$> narReadFile srcPath

