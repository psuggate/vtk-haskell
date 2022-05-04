{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, PatternSynonyms,
             ViewPatterns #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.Types
-- Copyright   : (C) Patrick Suggate, 2020
-- License     : BSD3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
-- Representations for various VTK data types.
--
-- == Changelog
--  - 25/10/2020  --  initial file;
--  - 08/07/2021  --  refactoring to improve generality;
--
------------------------------------------------------------------------------

module Data.VTK.Types
  (
    VtkCellType (..)
  , pattern VtkVertex
  , pattern VtkLine

  , pattern VtkTriangle
  , pattern VtkPolygon
  , pattern VtkPixel
  , pattern VtkQuad

  , pattern VtkTetra
  , pattern VtkVoxel
  , pattern VtkHexahedron
  , pattern VtkWedge
  , pattern VtkPyramid

  , pattern VtkQuadraticPyramid
  )
where

import           Control.DeepSeq  (NFData)
import           Foreign.Storable
import           GHC.Generics     (Generic)
import           GHC.Word


-- * VTK data types
------------------------------------------------------------------------------
-- | Type-codes for the supported VTK cell-types.
newtype VtkCellType
  = VtkCellType { getVtkCellType :: Word8 }
  deriving (Enum, Eq, Generic, NFData, Ord, Read, Show, Storable)


-- * Convenience patterns
------------------------------------------------------------------------------
pattern VtkVertex :: VtkCellType
pattern VtkVertex <- ((== VtkCellType 1) -> True)
  where VtkVertex  = VtkCellType 1

pattern VtkLine :: VtkCellType
pattern VtkLine <- ((== VtkCellType 3) -> True)
  where VtkLine  = VtkCellType 3

------------------------------------------------------------------------------
pattern VtkTriangle :: VtkCellType
pattern VtkTriangle <- ((== VtkCellType 5) -> True)
  where VtkTriangle  = VtkCellType 5

pattern VtkPolygon :: VtkCellType
pattern VtkPolygon <- ((== VtkCellType 7) -> True)
  where VtkPolygon  = VtkCellType 7

pattern VtkPixel :: VtkCellType
pattern VtkPixel <- ((== VtkCellType 8) -> True)
  where VtkPixel  = VtkCellType 8

pattern VtkQuad :: VtkCellType
pattern VtkQuad <- ((== VtkCellType 9) -> True)
  where VtkQuad  = VtkCellType 9

------------------------------------------------------------------------------
pattern VtkTetra :: VtkCellType
pattern VtkTetra <- ((== VtkCellType 10) -> True)
  where VtkTetra  = VtkCellType 10

pattern VtkVoxel :: VtkCellType
pattern VtkVoxel <- ((== VtkCellType 11) -> True)
  where VtkVoxel  = VtkCellType 11

pattern VtkHexahedron :: VtkCellType
pattern VtkHexahedron <- ((== VtkCellType 12) -> True)
  where VtkHexahedron  = VtkCellType 12

pattern VtkWedge :: VtkCellType
pattern VtkWedge <- ((== VtkCellType 13) -> True)
  where VtkWedge  = VtkCellType 13

pattern VtkPyramid :: VtkCellType
pattern VtkPyramid <- ((== VtkCellType 14) -> True)
  where VtkPyramid  = VtkCellType 14

------------------------------------------------------------------------------
pattern VtkQuadraticPyramid :: VtkCellType
pattern VtkQuadraticPyramid <- ((== VtkCellType 27) -> True)
  where VtkQuadraticPyramid  = VtkCellType 27


-- * Instances
------------------------------------------------------------------------------
instance Bounded VtkCellType where
  minBound = VtkVertex
  maxBound = VtkQuadraticPyramid
