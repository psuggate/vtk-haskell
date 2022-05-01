{-# LANGUAGE BangPatterns, DeriveAnyClass, DeriveGeneric, FlexibleInstances,
             OverloadedStrings, ScopedTypeVariables, TupleSections #-}

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
  , vtkArray

    -- Data types
  , DataArray (..)

    -- Exception-handling
  , VtkDataIOException (..)
  , typeErrorIO

    -- Conversions
  , fromVector
  , toVector
  , toVectorIO

  , setArrayProps
  )
where

import           Control.DeepSeq              (NFData)
import           Control.Monad                (when)
import           Control.Monad.ST
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as BL
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as Lazy
import qualified Data.Text.Lazy.Encoding      as Text
import           Data.VTK.DataArray.Internal
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
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | Exception handling when working with VTK arrays.
newtype VtkDataIOException
  = VtkDataIOException String
  deriving (Eq, Generic, Show)


-- * Instances for various array-element types
------------------------------------------------------------------------------
instance VtkArray (Vector Word8) where
  encodeArray l xs =
    DataArray "UInt8" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "UInt8" t 1 d n x

instance VtkArray (Vector VtkCellType) where
  encodeArray l xs =
    DataArray "UInt8" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "UInt8" t 1 d n x

------------------------------------------------------------------------------
instance VtkArray (Vector Int32) where
  encodeArray l xs =
    DataArray "Int32" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Int32" t 1 d n x

instance VtkArray (Vector Int64) where
  encodeArray l xs =
    DataArray "Int64" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Int64" t 1 d n x

instance VtkArray (Vector Int) where
  encodeArray l xs =
    DataArray "Int64" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Int64" t 1 d n x

------------------------------------------------------------------------------
instance VtkArray (Vector Float) where
  encodeArray l xs =
    DataArray "Float32" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float32" t 1 d n x

instance VtkArray (Vector (V3 Float)) where
  encodeArray l xs =
    DataArray "Float32" (Vec.length xs) 3 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float32" t 3 d n x

------------------------------------------------------------------------------
instance VtkArray (Vector Double) where
  encodeArray l xs =
    DataArray "Float64" (Vec.length xs) 1 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float64" t 1 d n x

instance VtkArray (Vector (V3 Double)) where
  encodeArray l xs =
    DataArray "Float64" (Vec.length xs) 3 l <$> fromVector xs
  decodeArray (DataArray t n d l x) = (l,) <$> toVectorIO "Float64" t 3 d n x


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

------------------------------------------------------------------------------
-- | Decode (from base64), decompress, then convert into an array of type @a@.
--
--   TODO:
--    + make faster, as this is fairly slow
--    + testbenches
--
toVector :: forall a. Storable a => Text -> Vector a
toVector ts
  | Lazy.null ts = Vec.empty
  | otherwise    = runST $ do
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
--    + compares @Text@ representations of types, array dimensions, and array
--      length (vs. the desired values) when deserialising the contents of an
--      array;
--
toVectorIO
  :: Storable a
  => Text -> Text
  -> Int -> Int
  -> Int
  -> Text
  -> IO (Vector a)
toVectorIO t0 t1 d0 d1 n ts = do
  -- print ts
  when (t0 /= t1) $ typeErrorIO t0 t1
  when (d0 /= d1) $ dimsErrorIO d0 d1
  let x = toVector ts
      m = Vec.length x
  if m /= n then sizeErrorIO n m else pure x
{-# INLINE[2] toVectorIO #-}


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
-- | Set the array-length and dimensions using the indicated element-type, and
--   array-data.
setArrayProps :: DataArray -> DataArray
setArrayProps (DataArray typ _ d label tx) =
  let xs = toVector tx :: Vector Word8
      n  = Vec.length xs `div` d
      s  = case typ of
        "UInt8"   -> 1
        "UInt16"  -> 2
        "UInt32"  -> 4
        "UInt64"  -> 8
        "Int8"    -> 1
        "Int16"   -> 2
        "Int32"   -> 4
        "Int64"   -> 8
        "Float32" -> 4
        "Float64" -> 8
        _         -> -1
  in  DataArray typ (n `div` s) d label tx


-- * Legacy
------------------------------------------------------------------------------
vtkArray :: VtkArray a => Text -> a -> IO DataArray
vtkArray  = encodeArray
{-# DEPRECATED vtkArray "replaced by 'encodeArray'" #-}
