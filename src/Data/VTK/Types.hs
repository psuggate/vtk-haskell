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
  , pattern VtkQuad
  , pattern VtkQuadraticPyramid
  )
where

import           Foreign.Storable
import           GHC.Generics     (Generic)
import           GHC.Word


-- * VTK data types
------------------------------------------------------------------------------
-- | Type-codes for the supported VTK cell-types.
newtype VtkCellType
  = VtkCellType { getVtkCellType :: Word8 }
  deriving (Enum, Eq, Generic, Ord, Read, Show, Storable)


-- * Convenience patterns
------------------------------------------------------------------------------
pattern VtkVertex :: VtkCellType
pattern VtkVertex <- ((== VtkCellType 1) -> True)
  where VtkVertex  = VtkCellType 1

pattern VtkLine :: VtkCellType
pattern VtkLine <- ((== VtkCellType 3) -> True)
  where VtkLine  = VtkCellType 3

pattern VtkTriangle :: VtkCellType
pattern VtkTriangle <- ((== VtkCellType 5) -> True)
  where VtkTriangle  = VtkCellType 5

pattern VtkQuad :: VtkCellType
pattern VtkQuad <- ((== VtkCellType 9) -> True)
  where VtkQuad  = VtkCellType 9

pattern VtkQuadraticPyramid :: VtkCellType
pattern VtkQuadraticPyramid <- ((== VtkCellType 27) -> True)
  where VtkQuadraticPyramid  = VtkCellType 27


-- * Instances
------------------------------------------------------------------------------
instance Bounded VtkCellType where
  minBound = VtkVertex
  maxBound = VtkQuadraticPyramid
