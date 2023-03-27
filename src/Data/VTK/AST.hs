{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, DerivingVia,
             GeneralisedNewtypeDeriving, TemplateHaskell #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.AST
-- Copyright   : (C) Patrick Suggate, 2022
-- License     : BSD3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
-- Abstract Syntax Tree (AST) for VTK files.
--
-- == Changelog
--  - 08/06/2022  --  initial file;
--
------------------------------------------------------------------------------

module Data.VTK.AST
  (
    module VTK
  , module Data.VTK.AST
  )
where

import           Control.DeepSeq (NFData)
import           Control.Lens    (makeClassy, makeLenses)
import           Data.Text       (Text)
import           Data.VTK.Core   as VTK hiding (Piece (..))
import           GHC.Generics    (Generic)


-- * VTK AST data types
------------------------------------------------------------------------------
newtype VtkDataset
  = VtkDataset { vtkfiles :: [(FilePath, VtkFile)] }
  deriving newtype (Eq, NFData, Show)
  deriving (Generic)

data VtkFile
  = VtkFile
      { vtkMeta        :: VtkMeta
      , datasetElement :: DatasetElement
      }
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | VTK file metadata.
data VtkMeta
  = VtkMeta
      { _fileType   :: VtkFileType
      , _version    :: (Int, Int)
      , _byteOrder  :: ByteOrder
      , _compressor :: CompressType
      }
  deriving (Eq, Generic, NFData, Show)

newtype VtkFileType
  = VtkFileType { unVtkFileType :: Text }
  deriving (Read, Show)
    via Text
  deriving newtype (Eq, NFData)
  deriving (Generic)

data ByteOrder
  = BigEndian
  | LittleEndian
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Read, Show)

data CompressType
  = ZLib
  | None
  deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)


-- ** Common data types
------------------------------------------------------------------------------
-- | Axis-Aligned Bounding-Box (AABB) representing the geometric/parametric
--   extent of a VTK mesh (piece).
data Extent
  = Extent Int Int Int Int Int Int
  deriving (Eq, Generic, NFData, Show)

data Spacing
  = Spacing !Double !Double !Double
  deriving (Eq, Generic, NFData, Show)

data Origin
  = Origin !Double !Double !Double
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
data DatasetElement
  = ImageData Extent Origin Spacing [ImageDataPiece]
  | RectilinearGrid Extent [RectilinearGridPiece]
  | StructuredGrid Extent [StructuredGridPiece]
  | PolyData [PolyDataPiece]
  | UnstructuredGrid [UnstructuredGridPiece]
  deriving (Eq, Generic, NFData, Show)


-- ** Image data types
------------------------------------------------------------------------------
data ImageDataPiece
  = ImageDataPiece !Extent !PointData !CellData
  deriving (Eq, Generic, NFData, Show)


-- ** Rectilinear grid types
------------------------------------------------------------------------------
data RectilinearGridPiece
  = RectilinearGridPiece !Extent !Coordinates !PointData !CellData
  deriving (Eq, Generic, NFData, Show)


-- ** Structured-grip types
------------------------------------------------------------------------------
data StructuredGridPiece
  = StructuredGridPiece !Extent !Points !PointData !CellData
  deriving (Eq, Generic, NFData, Show)


-- ** Polygon mesh data types
------------------------------------------------------------------------------
data PolyDataPiece
  = PolyDataPiece PolyDataPieceMeta PointData CellData Points Verts Lines Strips Polys
  deriving (Eq, Generic, NFData, Show)

data PolyDataPieceMeta
  = PolyDataPieceMeta
      { polyDataPieceMetaNumberOfPoints :: !Int
      , polyDataPieceMetaNumberOfVerts  :: !Int
      , polyDataPieceMetaNumberOfLines  :: !Int
      , polyDataPieceMetaNumberOfStrips :: !Int
      , polyDataPieceMetaNumberOfPolys  :: !Int
      }
  deriving (Eq, Generic, NFData, Show)


-- ** Unstructured grid types
------------------------------------------------------------------------------
data UnstructuredGridPiece
  = UnstructuredGridPiece
      { unstructuredGridPieceMetadata  :: !UnstructuredGridPieceMeta
      , unstructuredGridPiecePointData :: !PointData
      , unstructuredGridPieceCellData  :: !CellData
      , unstructuredGridPiecePoints    :: !Points
      , unstructuredGridPieceCells     :: !Cells
      }
  deriving (Eq, Generic, NFData, Show)

data UnstructuredGridPieceMeta
  = UnstructuredGridPieceMeta
      { unstructuredGridPieceMetaNumberOfPoints :: !Int
      , unstructuredGridPieceMetaNumberOfCells  :: !Int
      }
  deriving (Eq, Generic, NFData, Show)


-- * Lenses & instances
------------------------------------------------------------------------------
makeLenses ''VtkMeta

makeClassy ''PolyDataPieceMeta
makeClassy ''UnstructuredGridPiece
makeClassy ''UnstructuredGridPieceMeta
