{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables, TupleSections #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.DataArray
-- Copyright   : (C) Patrick Suggate, 2020
-- License     : BSD3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
-- Representations and functions for VTK, data-arrays.
--
-- == Changelog
--  - 24/10/2020  --  initial file;
--
------------------------------------------------------------------------------

module Data.VTK.DataArray
  (
    -- Type classes
    VtkArray (..)

    -- Data types
  , DataArray (..)

    -- Exception-handling
  , VtkDataIOException (..)
  , typeErrorIO

    -- Conversions
  , fromVector
  , toVector

  , decompress
  )
where

import qualified Codec.Compression.Zlib       as Z
import           Control.Arrow                (second)
import           Control.Monad                (when)
import           Control.Monad.ST
import           Data.Bits                    (Bits, unsafeShiftL, unsafeShiftR,
                                               (.&.))
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Base64  as BL
import qualified Data.Text                    as Text
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as Lazy
import qualified Data.Text.Lazy.Encoding      as Text
import           Data.VTK.Types
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as Vec
import qualified Data.Vector.Storable.Mutable as Mut
import           Foreign.Ptr                  (castPtr)
import           Foreign.Storable
import           GHC.Generics                 (Generic)
import           GHC.Int
import           GHC.Word
import           Linear                       (V3 (..))
import           Text.Printf
import           UnliftIO.Exception           (Exception, throwIO)


-- * Serialisation type-classes
------------------------------------------------------------------------------
class VtkArray a where
  encodeArray :: Text -> a -> IO DataArray
  decodeArray :: DataArray -> IO (Text, a)


-- * VTK data-array types
------------------------------------------------------------------------------
-- | Array of (encoded, compressed) VTK data.
--
--   TODO:
--    + what is the best representation for this: @ByteString@, @TextShort@,
--      @Lazy.Text@, (strict) @Text@?
--
data DataArray
  = DataArray
      { arrayType :: !Text
      , arraySize :: !Int
      , arrayDims :: !Int
      , arrayName :: !Text
      , arrayData :: !Text
      }
  deriving (Generic)

------------------------------------------------------------------------------
-- | Exception handling when working with VTK arrays.
newtype VtkDataIOException
  = VtkDataIOException String
  deriving (Eq, Show)


-- * Instances for various array-element types
------------------------------------------------------------------------------
instance VtkArray (Vector Word8) where
  encodeArray l xs =
    DataArray "UInt8" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "UInt8" t n 1 d x

instance VtkArray (Vector VtkCellType) where
  encodeArray l xs =
    DataArray "UInt8" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "UInt8" t n 1 d x

------------------------------------------------------------------------------
instance VtkArray (Vector Int32) where
  encodeArray l xs =
    DataArray "Int32" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Int32" t n 1 d x

instance VtkArray (Vector Int64) where
  encodeArray l xs =
    DataArray "Int64" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Int64" t n 1 d x

instance VtkArray (Vector Int) where
  encodeArray l xs =
    DataArray "Int64" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Int64" t n 1 d x

------------------------------------------------------------------------------
instance VtkArray (Vector Float) where
  encodeArray l xs =
    DataArray "Float32" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float32" t n 1 d x

instance VtkArray (Vector (V3 Float)) where
  encodeArray l xs =
    DataArray "Float32" (Vec.length xs) 3 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float32" t n 3 d x

------------------------------------------------------------------------------
instance VtkArray (Vector Double) where
  encodeArray l xs =
    DataArray "Float64" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float64" t n 1 d x

instance VtkArray (Vector (V3 Double)) where
  encodeArray l xs =
    DataArray "Float64" (Vec.length xs) 3 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float64" t n 3 d x


-- ** Exception-handling
------------------------------------------------------------------------------
instance Exception VtkDataIOException


-- * Raw data
------------------------------------------------------------------------------
fromVector :: forall a. Storable a => Vector a -> IO Text
fromVector xs = Vec.unsafeWith xs $ \p -> do
  let n = sizeOf (undefined :: a) * Vec.length xs
      f q = BS.memcpy q (castPtr p) n
      {-# INLINE f #-}
  compressPlusHeader . BL.fromStrict <$> BS.create n f
{-# INLINE[2] fromVector #-}

-- todo: make faster, as this is fairly slow
toVector :: forall a. Storable a => Text -> Vector a
toVector ts = runST $ do
  let xs = decompress $ Text.encodeUtf8 ts
      n  = fromIntegral $ BL.length xs
  ar <- Mut.new n
  let go !i
        | i  <  n   = Mut.unsafeWrite ar i (BL.index xs (fromIntegral i)) *> go (i+1)
        | otherwise = pure ()
  go 0
  Vec.unsafeCast <$> Vec.unsafeFreeze ar
{-# INLINABLE[2] toVector #-}


-- ** Safer functions for working with raw data
------------------------------------------------------------------------------
-- | Array deserialisation with exception-handling.
--
--   NOTE:
--     + compares @Text@ representations of types, array dimensions, and array
--       length (vs. the desired values) when deserialising the contents of an
--       array;
--
toVectorIO
  :: Storable a
  => Text -> Text
  -> Int -> Int
  -> Int
  -> Text
  -> IO (Vector a)
toVectorIO t0 t1 d0 d1 n ts = do
  when (t0 /= t1) $ typeErrorIO t0 t1
  when (d0 /= d1) $ dimsErrorIO d0 d1
  let x = toVector ts
      m = Vec.length x
  if m /= n then sizeErrorIO n m else pure x
{-# INLINE[2] toVectorIO #-}


-- * Compression
------------------------------------------------------------------------------
-- | Break the given @ByteString@ into chunks, compress these, and then pre-
--   pend a header.
--
--   NOTE:
--    + see 'sc_vtk_write_compressed' (from 'sc_io.c') for the source-code
--      that was used to determine the header format;
--
compressPlusHeader :: BL.ByteString -> Text
compressPlusHeader bs =
  let s  = 1 `unsafeShiftL` 15           -- block-size
      l  = BL.length bs .&. pred s       -- 'lastsize'
      n  = (BL.length bs + s - 1) `unsafeShiftR` 15 -- num blocks
      o  = if l > 0 || BL.null bs then l else s

      -- Break into 32 KiB blocks, and then Z-compress
      cs = Z.compress <$> bchunks s bs

      -- Compute the header structure
      os = Vec.fromList . map w32 $ [n, s, o] ++ BL.length `map` cs
      hx = BL.pack . Vec.toList $ Vec.unsafeCast os

      -- Base64 encode header plus each block seperately, before concatenating
  in  BL.encodeBase64 hx <> BL.encodeBase64 (BL.concat cs)

decompress :: BL.ByteString -> BL.ByteString
decompress bx = BL.concat cs where
  bs = case BL.decodeBase64 bx of
    Left er -> error $ Text.unpack er
    Right x -> x
  n  = btoi 4 0 bs :: Int64
  s  = btoi 4 4 bs :: Int64
  -- l  = btoi 4 8 bs :: Int64 -- unused: size of last chunk
  h  = 4*(n+3) -- header size
  cs = Z.decompress <$> bchunks s (BL.drop h bs)


-- * Exception handling
------------------------------------------------------------------------------
typeErrorIO :: forall a. Text -> Text -> IO a
typeErrorIO t = throwIO . VtkDataIOException .
  printf "array types do not match (%s /= %s)" (Lazy.unpack t) . Lazy.unpack

dimsErrorIO :: forall a. Int -> Int -> IO a
dimsErrorIO d = throwIO . VtkDataIOException .
  printf "array dimensions do not match (%d /= %d)" d

sizeErrorIO :: forall a. Int -> Int -> IO a
sizeErrorIO d = throwIO . VtkDataIOException .
  printf "array lengths do not match (%d /= %d)" d


-- * Helpers
------------------------------------------------------------------------------
-- | Break the given @ByteString@ into @n@-sized chunks.
bchunks :: Int64 -> BL.ByteString -> [BL.ByteString]
bchunks n = go where
  go :: BL.ByteString -> [BL.ByteString]
  go !bs | BL.null bs = []
         | otherwise  = uncurry (:) . second go $ BL.splitAt n bs
{-# INLINE bchunks #-}

w32 :: Integral i => i -> Word32
w32  = fromIntegral
{-# INLINE w32 #-}

------------------------------------------------------------------------------
-- | @ByteString@ to an @Integral@ type.
btoi :: forall i. (Bits i, Integral i) => Int64 -> Int64 -> BL.ByteString -> i
btoi n i bs = go 0 n i where
  go :: i -> Int64 -> Int64 -> i
  go !x 0 _ = x
  go !x c j = go (unsafeShiftL x 8 + fromIntegral (BL.index bs j)) (c-1) (j+1)
