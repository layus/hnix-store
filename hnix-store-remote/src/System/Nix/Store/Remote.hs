{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
module System.Nix.Store.Remote
  (
    addToStore
  , addToStoreNar
  , addTextToStore
  , addSignatures
  , addIndirectRoot
  , addTempRoot
  , buildPaths
  , ensurePath
  , findRoots
  , isValidPathUncached
  , queryValidPaths
  , queryAllValidPaths
  , querySubstitutablePaths
  , queryPathInfoUncached
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , queryMissing
  , optimiseStore
  , runStore
  , syncWithGC
  , verifyStore
  )
  where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Binary.Put           as B
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map.Strict           as M
import           Data.Proxy                (Proxy)
import           Data.Text                 (Text)

import qualified System.Nix.Build          as Build

import           System.Nix.Hash           (Digest, ValidAlgo)
import           System.Nix.StorePath
import           System.Nix.Hash
import           System.Nix.Nar            (localPackNar, putNar, narEffectsIO, Nar)
import           System.Nix.ValidPath

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

import Data.Text.Encoding (encodeUtf8)

type RepairFlag = Bool
type CheckFlag = Bool
type CheckSigsFlag = Bool
type SubstituteFlag = Bool

addToStore
  :: forall a. (ValidAlgo a, NamedAlgo a)
  => StorePathName
  -> FilePath
  -> Bool
  -> Proxy a
  -> (StorePath -> Bool)
  -> RepairFlag
  -> MonadStore StorePath
addToStore name pth recursive _algoProxy pfilter repair = do

  -- TODO: Is this lazy enough? We need `B.putLazyByteString bs` to stream `bs`
  bs  :: BSL.ByteString <- liftIO $ B.runPut . putNar <$> localPackNar narEffectsIO pth

  runOpArgs AddToStore $ do
    putText $ unStorePathName name

    putBool $ not $ algoName @a == "sha256" && recursive
    putBool recursive

    putText $ algoName @a

    B.putLazyByteString bs

  sockGetPath

addToStoreNar :: ValidPath -> Nar -> RepairFlag -> CheckSigsFlag -> MonadStore ()
addToStoreNar ValidPath{..} nar repair checkSigs = do
  -- after the command, protocol asks for data via Read message
  -- so we provide it here
  let n = B.runPut $ putNar nar
  setData n

  void $ runOpArgs AddToStoreNar $ do
    putPath path
    maybe (putText "") (putPath) deriver
    putText narHash
    putPaths references
    putTime registrationTime
    putInt narSize
    putBool ultimate
    putTexts sigs
    putText ca

    putBool repair
    putBool (not checkSigs)

-- reference accepts repair but only uses it to throw error in case of nix daemon
addTextToStore :: Text
               -> Text
               -> StorePathSet
               -> RepairFlag
               -> MonadStore StorePath
addTextToStore name text references' repair = do
  when repair $ error "repairing is not supported when building through the Nix daemon"
  runOpArgs AddTextToStore $ do
    putText name
    putText text
    putPaths references'
  sockGetPath

addSignatures :: StorePath -> [BSL.ByteString] -> MonadStore ()
addSignatures p signatures = do
  void $ simpleOpArgs AddSignatures $ do
    putPath p
    putByteStrings signatures

addIndirectRoot :: StorePath -> MonadStore ()
addIndirectRoot pn = do
  void $ simpleOpArgs AddIndirectRoot $ putPath pn

addTempRoot :: StorePath -> MonadStore ()
addTempRoot pn = do
  void $ simpleOpArgs AddTempRoot $ putPath pn

buildPaths :: StorePathSet -> Build.BuildMode -> MonadStore ()
buildPaths ps bm = do
  void $ simpleOpArgs BuildPaths $ do
    putPaths ps
    putInt $ fromEnum bm

ensurePath :: StorePath -> MonadStore ()
ensurePath pn = do
  void $ simpleOpArgs EnsurePath $ putPath pn

findRoots :: MonadStore (M.Map ByteString StorePath)
findRoots = do
  runOp FindRoots
  sd <- getStoreDir
  res <- getSocketIncremental $ getMany $ (,) <$> getByteStringLen <*> getPath sd

  r <- catRights res
  return $ M.fromList $ r
  where
    catRights :: [(a, Either String b)] -> MonadStore [(a, b)]
    catRights = mapM ex

    ex :: (a, Either [Char] b) -> MonadStore (a, b)
    ex (x, Right y) = return (x, y)
    ex (_x , Left e) = throwError $ "Unable to decode root: "  ++ e


isValidPathUncached :: StorePath -> MonadStore Bool
isValidPathUncached p = do
  simpleOpArgs IsValidPath $ putPath p

queryValidPaths :: StorePathSet -> SubstituteFlag -> MonadStore StorePathSet
queryValidPaths ps substitute = do
  runOpArgs QueryValidPaths $ do
    putPaths ps
    putBool substitute
  sockGetPaths

queryAllValidPaths :: MonadStore StorePathSet
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: StorePathSet -> MonadStore StorePathSet
querySubstitutablePaths ps = do
  runOpArgs QuerySubstitutablePaths $ do
    putPaths ps
  sockGetPaths

queryPathInfoUncached :: StorePath -> MonadStore ValidPath
queryPathInfoUncached path = do
  runOpArgs QueryPathInfo $ do
    putPath path

  valid <- sockGetBool
  unless valid $ error "Path is not valid"

  deriver <- sockGetPathMay
  narHash <- bsToText <$> sockGetStr
  references <- sockGetPaths
  registrationTime <- sockGet getTime
  narSize <- sockGetInt
  ultimate <- sockGetBool
  sigs <- map bsToText <$> sockGetStrings
  ca <- bsToText <$> sockGetStr
  return $ ValidPath {..}

queryReferrers :: StorePath -> MonadStore StorePathSet
queryReferrers p = do
  runOpArgs QueryReferrers $ do
    putPath p
  sockGetPaths

queryValidDerivers :: StorePath -> MonadStore StorePathSet
queryValidDerivers p = do
  runOpArgs QueryValidDerivers $ do
    putPath p
  sockGetPaths

queryDerivationOutputs :: StorePath -> MonadStore StorePathSet
queryDerivationOutputs p = do
  runOpArgs QueryDerivationOutputs $
    putPath p
  sockGetPaths

queryDerivationOutputNames :: StorePath -> MonadStore StorePathSet
queryDerivationOutputNames p = do
  runOpArgs QueryDerivationOutputNames $
    putPath p
  sockGetPaths

queryPathFromHashPart :: Digest StorePathHashAlgo -> MonadStore StorePath
queryPathFromHashPart storePathHash = do
  runOpArgs QueryPathFromHashPart $
    putByteStringLen $ BSL.fromStrict $ encodeUtf8 $ encodeBase32 storePathHash
  sockGetPath

queryMissing :: StorePathSet -> MonadStore (StorePathSet, StorePathSet, StorePathSet, Integer, Integer)
queryMissing ps = do
  runOpArgs QueryMissing $ do
    putPaths ps

  willBuild <- sockGetPaths
  willSubstitute <- sockGetPaths
  unknown <- sockGetPaths
  downloadSize' <- sockGetInt
  narSize' <- sockGetInt
  return (willBuild, willSubstitute, unknown, downloadSize', narSize')

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

syncWithGC ::MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair
