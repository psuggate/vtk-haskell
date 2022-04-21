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
      headerLength bs `shouldBe` 16

    it "can extract the header & body of compressed binary data" $ do
      let (hx, bx) = splitAtHeader bs
      hx `shouldBe` [1, 32768, 4, 12]
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

    it "can encode then decode arrays that make a unit square" $ do
      let xs = Vec.fromList [0.0, 1.0, 1.0, 0.0 :: Float]
          ys = Vec.fromList [0.0, 0.0, 1.0, 1.0 :: Float]
          zs = Vec.fromList [0.0, 0.0, 0.0, 0.0 :: Float]
      dx <- encodeArray "xcoords" xs >>= fmap snd . decodeArray
      dx `shouldBe` xs
      dy <- encodeArray "ycoords" ys >>= fmap snd . decodeArray
      dy `shouldBe` ys
      dz <- encodeArray "zcoords" zs >>= fmap snd . decodeArray
      dz `shouldBe` zs

      let cx = Vec.fromList [0, 1, 2, 3 :: Int32]
          ox = Vec.fromList [4 :: Int32]
          tx = Vec.singleton VtkQuad
      ci <- encodeArray "connectivity" cx >>= fmap snd . decodeArray
      ci `shouldBe` cx
      os <- encodeArray "offsets" ox >>= fmap snd . decodeArray
      os `shouldBe` ox
      ts <- encodeArray "types"   tx >>= fmap snd . decodeArray
      ts `shouldBe` tx
