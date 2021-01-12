{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.AsciiData
-- Copyright   : (C) Patrick Suggate, 2020
-- License     : BSD3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
-- Representations and functions for ASCII representations of VTK, data-
-- arrays, and primarily for testing purposes.
--
-- == Changelog
--  - 26/10/2020  --  initial file;
--
------------------------------------------------------------------------------

module Data.VTK.AsciiData
  (
    -- Type classes
    VtkAscii (..)

    -- Data types
  , DataArray (..)

    -- Conversions
  , vectorToAscii
  )
where

import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as L
import           Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as Vec
import           Data.VTK.DataArray
import           Data.VTK.Types
import           GHC.Int
import           GHC.Word
import           Linear               (V3 (..))


-- * Serialisation type-classes
------------------------------------------------------------------------------
class VtkAscii a where
  vtkAscii :: Text -> a -> DataArray


-- * Instances
------------------------------------------------------------------------------
instance VtkAscii (Vector Word8) where
  vtkAscii l xs =
    DataArray "UInt8" (Vec.length xs) 1 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

instance VtkAscii (Vector VtkCellType) where
  vtkAscii l xs =
    DataArray "UInt8" (Vec.length xs) 1 l . vectorToAscii $ Vec.map ui8 xs
  {-# INLINE[2] vtkAscii #-}

------------------------------------------------------------------------------
instance VtkAscii (Vector Int32) where
  vtkAscii l xs =
    DataArray "Int32" (Vec.length xs) 1 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

instance VtkAscii (Vector Int64) where
  vtkAscii l xs =
    DataArray "Int64" (Vec.length xs) 1 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

instance VtkAscii (Vector Int) where
  vtkAscii l xs =
    DataArray "Int64" (Vec.length xs) 1 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

------------------------------------------------------------------------------
instance VtkAscii (Vector Float) where
  vtkAscii l xs =
    DataArray "Float32" (Vec.length xs) 1 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

instance VtkAscii (Vector (V3 Float)) where
  vtkAscii l xs =
    DataArray "Float32" (Vec.length xs) 3 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

------------------------------------------------------------------------------
instance VtkAscii (Vector Double) where
  vtkAscii l xs =
    DataArray "Float64" (Vec.length xs) 1 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}

instance VtkAscii (Vector (V3 Double)) where
  vtkAscii l xs =
    DataArray "Float64" (Vec.length xs) 3 l $ vectorToAscii xs
  {-# INLINE[2] vtkAscii #-}


-- * Raw data
------------------------------------------------------------------------------
vectorToAscii :: forall a. (Storable a, Show a) => Vector a -> Text
vectorToAscii  = L.unwords . map (L.pack . show) . Vec.toList
{-# INLINE[1] vectorToAscii #-}


-- * Helpers
------------------------------------------------------------------------------
ui8 :: Enum e => e -> Word8
ui8  = fromIntegral . fromEnum
{-# INLINE ui8 #-}
