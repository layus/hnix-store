{-|
Description : Representation of Nix store paths.
-}
module System.Nix.StorePath
  ( -- * Basic store path types
    StorePath(..)
  , StorePathName
  , StorePathSet
  , StorePathHashAlgo
  , ContentAddressableAddress(..)
  , NarHashMode(..)
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , unStorePathName
  , validStorePathName
  , -- * Rendering out 'StorePath's
    storePathToRawFilePath
  , parsePath
  ) where

import System.Nix.Internal.StorePath
