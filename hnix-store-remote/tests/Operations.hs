{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Operations where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe

import           Data.Proxy
import           Data.Text.Encoding    ( encodeUtf8 )
import           Data.Text            as T
import           Data.Text.Encoding   as T
import           Data.Text.Lazy       as TL
import           Data.Text.Lazy.Encoding as TL
import           Data.ByteString.Char8 as BS

import           System.FilePath
import           System.Nix.Hash
import           System.Nix.Internal.Hash
import           System.Nix.Nar
import           System.Nix.StorePath
import           System.Nix.Store.Remote
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util
import           System.Nix.Util

import           Test.Tasty                  as T
import           Test.Tasty.Hspec
import qualified Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck
import           Text.Read                   (readMaybe)

spec_addToStore :: Spec
spec_addToStore = do

  describe "addToStore remote operation" $ do

    it "uploads correctly" $ do
      let name = fromJust $ makeStorePathName "test-recursive-add"
      let srcPath = "./tests/data/add-recursive"
      let recursive = True
      let filter :: FilePathFilter
          filter path | takeBaseName path == "ignore" = False
                      | otherwise = True
      let repair = False
      res <- runStore $ (addToStore @'SHA256 name srcPath recursive filter repair :: MonadStore "/nix/store" (StorePath "/nix/store"))
      show (fmap show res) `shouldBe` (Right "/nix/store/5x7ijzhz59rf3ldwp1qy28sz0abhcqd1-test-recursive-add",[Last])

