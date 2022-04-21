{-# LANGUAGE OverloadedStrings #-}

module Data.VTK.DataArraySpec where

import qualified Data.ByteString.Lazy        as BL
import           Data.Int
import qualified Data.VTK.DataArray          as DA
import           Data.VTK.DataArray.Internal as DA
import           Data.VTK.Unstructured       as Unst
import qualified Data.Vector.Storable        as Vec
import           Data.Word
import qualified System.Random               as Random
import           Test.Hspec


-- * Top-level tests
------------------------------------------------------------------------------
spec :: Spec
spec  = context "VTK data arrays" $ do
  let xu = Vec.singleton 1 :: Vec.Vector Int32
      bs = "AQAAAACAAAAEAAAADAAAAA==eJxjZGBgAAAACAAC"

  describe "Tests for conversions to/from binary arrays" $ do

    it "ByteString to Integer works correctly" $ do
      let bx = "\0\0\0\0AAAABBBB"
      DA.btoi 4 0 bx `shouldBe` (0 :: Word32)
      DA.btoiBE 4 1 bx `shouldBe` (65 :: Word32)
      DA.btoi 1 4 bx `shouldBe` (65 :: Int64)
      DA.btoi 2 4 bx `shouldBe` (65*256 + 65 :: Int64)
      DA.btoi 3 4 bx `shouldBe` (65*65536 + 65*256 + 65 :: Int64)
      DA.btoi 4 4 bx `shouldBe` (65*16777216 + 65*65536 + 65*256 + 65 :: Int64)

    it "can compute header size of compressed binary data" $ do
      let lh = headerLength bs
      -- print $ BL.length . BL.decodeBase64Lenient $ BL.takeWhile (/= eq) bs
      lh `shouldBe` 16

    it "can extract the header values of compressed binary data" $ do
      let hx = headerValues bs
      -- print hx
      hx `shouldBe` ([1, 32768, 4, 12], 24)

    it "can extract the header of compressed binary data" $ do
      let bx = snd $ splitAtHeader bs
      -- print bx
      bx `shouldBe` BL.drop 24 bs

    it "can convert between Haskell Vectors and DataArrays" $ do
      xs <- DA.toVector <$> DA.fromVector xu
      xs `shouldBe` xu

  describe "DataArray encoding tests" $ do

    it "can encode then decode a single-element array of Int32" $ do
      let xr = Vec.singleton 1 :: Vec.Vector Int32
      xa <- encodeArray "xcoords" xr
      -- print xa
      xs <- snd <$> decodeArray xa
      xs `shouldBe` xr

    it "can encode then decode an array of Float32" $ do
      sg <- Random.newStdGen
      let xr = Vec.fromList . take 16 $ Random.randoms sg :: Vec.Vector Float
      xa <- encodeArray "xcoords" xr
      xs <- snd <$> decodeArray xa
      xs `shouldBe` xr
