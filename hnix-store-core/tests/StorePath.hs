{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module StorePath where
import qualified Data.ByteString.Char8       as BSC

import           Test.Tasty.QuickCheck

import           System.Nix.Internal.StorePath
import           Arbitrary

-- | Test that Nix(OS) like paths roundtrip
prop_storePathRoundtrip (x' :: NixLike) = \(NixLike x) ->
  (parsePath "/nix/store" $ storePathToRawFilePath x) === Right x

-- | Test that any `StorePath` roundtrips
prop_storePathRoundtrip' x =
  (parsePath (storePathRoot x) $ storePathToRawFilePath x) === Right x
