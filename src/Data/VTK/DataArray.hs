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
-- Changelog:
--  + 24/10/2020  --  initial file;
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
  )
where

import qualified Codec.Compression.Zlib      as Z
import           Control.Arrow               (second)
import           Data.Bits                   (unsafeShiftL, unsafeShiftR, (.&.))
import qualified Data.ByteString.Internal    as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Base64 as BL
import           Data.Text.Lazy              (Text)
import           Data.Vector.Storable        (Vector)
import qualified Data.Vector.Storable        as Vec
import           Data.VTK.Types
import           Foreign.Ptr                 (castPtr)
import           Foreign.Storable
import           GHC.Generics                (Generic)
import           GHC.Int
import           GHC.Word
import           Linear                      (V3 (..))

-- import           Data.Bits.Handy             ((.&.), (<<%), (>>%))


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
--   BL.encodeBase64 . Z.compress . BL.fromStrict <$> BS.create n f
  compressPlusHeader . BL.fromStrict <$> BS.create n f
{-# INLINE[1] fromVector #-}

{-- }
fromText :: forall a. Storable a => Text -> IO (Vector a)
fromText ts = do
--}


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
