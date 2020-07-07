{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Api.TextView
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.Api.TextView
import           Cardano.Api.Shelley.ColdKeys

import qualified Data.ByteString.Char8 as BS

import           Hedgehog (Property, discover)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Cardano.Config.Gen


prop_roundtrip_shelley_SigningKey_view :: Property
prop_roundtrip_shelley_SigningKey_view =
  Hedgehog.property $ do
    kr <- Hedgehog.forAll genKeyRole
    sk <- Hedgehog.forAll genSigningKey
    Hedgehog.tripping sk (encodeSigningKey kr) (decodeSigningKey kr)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
