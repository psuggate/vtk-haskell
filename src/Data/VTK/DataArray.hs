{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables #-}

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

    -- Conversions
  , fromVector
  , toVector

  , decompress
  )
where

import qualified Codec.Compression.Zlib       as Z
import           Control.Arrow                (second)
import           Control.Monad.ST
import           Data.Bits                    (Bits, unsafeShiftL, unsafeShiftR,
                                               (.&.))
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Base64  as BL
import qualified Data.Text                    as Text
import           Data.Text.Lazy               (Text)
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


-- * Serialisation type-classes
------------------------------------------------------------------------------
class VtkArray a where
  vtkArray :: Text -> a -> IO DataArray


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


-- * Instances
------------------------------------------------------------------------------
instance VtkArray (Vector Word8) where
  vtkArray l xs =
    DataArray "UInt8" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

instance VtkArray (Vector VtkCellType) where
  vtkArray l xs =
    DataArray "UInt8" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

------------------------------------------------------------------------------
instance VtkArray (Vector Int32) where
  vtkArray l xs =
    DataArray "Int32" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

instance VtkArray (Vector Int64) where
  vtkArray l xs =
    DataArray "Int64" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

instance VtkArray (Vector Int) where
  vtkArray l xs =
    DataArray "Int64" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

------------------------------------------------------------------------------
instance VtkArray (Vector Float) where
  vtkArray l xs =
    DataArray "Float32" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

instance VtkArray (Vector (V3 Float)) where
  vtkArray l xs =
    DataArray "Float32" (Vec.length xs) 3 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

------------------------------------------------------------------------------
instance VtkArray (Vector Double) where
  vtkArray l xs =
    DataArray "Float64" (Vec.length xs) 1 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}

instance VtkArray (Vector (V3 Double)) where
  vtkArray l xs =
    DataArray "Float64" (Vec.length xs) 3 l <$> fromVector xs
  {-# INLINE[2] vtkArray #-}


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
