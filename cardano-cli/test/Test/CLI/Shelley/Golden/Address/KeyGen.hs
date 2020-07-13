{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.CLI.Shelley.Golden.Address.KeyGen
  ( golden_shelleyAddressKeyGen
  ) where

import Cardano.Prelude hiding (to)

import qualified System.Directory as IO
import qualified System.IO.Temp as IO

import Hedgehog (Property)

import qualified Hedgehog as H
import qualified Test.OptParse as OP

{- HLINT ignore "Use camelCase" -}

golden_shelleyAddressKeyGen :: Property
golden_shelleyAddressKeyGen = OP.propertyOnce $ do
  liftIO $ IO.createDirectoryIfMissing True "tmp/Address/KeyGen"
  tempDir <- liftIO $ IO.createTempDirectory "tmp/Address/KeyGen" "test"
  H.annotate $ "Temporary directory: " <> tempDir
  let addressVKeyFile = tempDir <> "/address.vkey"
  let addressSKeyFile = tempDir <> "/address.skey"
  let outputFiles = [addressVKeyFile, addressSKeyFile]

  OP.execCardanoCLIParser outputFiles $
    OP.evalCardanoCLIParser
      [ "shelley","address","key-gen"
      , "--verification-key-file", addressVKeyFile
      , "--signing-key-file", addressSKeyFile
      ]

  OP.assertFilesExist outputFiles

  OP.assertFileOccurences 1 "PaymentVerificationKeyShelley" addressVKeyFile
  OP.assertFileOccurences 1 "SigningKeyShelley" addressSKeyFile

  liftIO $ IO.removeDirectoryRecursive tempDir
