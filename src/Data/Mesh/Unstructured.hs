{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, FlexibleInstances, GADTs,
             GeneralisedNewtypeDeriving, KindSignatures, LambdaCase,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell,
             TypeFamilies, TypeOperators #-}

module Data.Mesh.Unstructured where

import           Control.DeepSeq      (NFData)
import           Control.Lens         (makeLenses)
import           Data.Text.Lazy       (Text)
import           Data.VTK.Types
import           Data.Vector.Storable (Vector)
import           GHC.Generics         (Generic)
import           Linear               (V3)

-- import GHC.Word


-- * Unstructured-grid data types
------------------------------------------------------------------------------
-- | Top-level, unstructured-grid data type.
--
--   TODO:
--    + non-default attributes;
--    + proper support for multiple pieces;
--    + 3D "meshes";
--    + only support @Double@-precision floating-point?
--
data UnstructuredGrid a where
  UnstructuredGrid :: Piece t -> UnstructuredGrid ts -> UnstructuredGrid (t : ts)
  Nil :: UnstructuredGrid '[]

------------------------------------------------------------------------------
-- | Mesh substructures.
data Piece t
  = Piece
      { piecePoints    :: !(Points t)
      , piecePointData :: !(PointData t)
      , pieceCells     :: !Cells
      , pieceCellData  :: !(CellData t)
      , pieceVerts     :: !Verts
      , pieceLines     :: !Lines
      , pieceStrips    :: !Strips
      , piecePolys     :: !Polys
      }
  deriving (Eq, Generic, NFData, Show)


-- ** Arrays used by the mesh
------------------------------------------------------------------------------
data DataArray a
  = DataArray
      { _nameOf :: !Text
      , _dataOf :: !(Vector a)
      }
  deriving (Eq, Generic, NFData, Show)


-- ** Vertex data
------------------------------------------------------------------------------
data Points a
  = PointsChunked !(DataArray (V3 a))
  | PointsStriped !(Coordinates a)
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | Coordinates of vertices ('Points') are required to have three (spatial)
--   components.
data Coordinates a
  = Coordinates
      { _xcoords :: !(DataArray a)
      , _ycoords :: !(DataArray a)
      , _zcoords :: !(DataArray a)
      }
  deriving (Eq, Generic, NFData, Show)

data PointData a
  = PointData
      { pscalars :: [DataArray a]
      , pvectors :: [DataArray a]
      , pnormals :: [DataArray a]
      , ptensors :: [DataArray a]
      , ptcoords :: [DataArray a]
      }
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | Topological vertices.
data Verts
  = Verts
      { vertConnect :: !(DataArray Int)
      , vertOffsets :: !(DataArray Int)
      }
  | NoVerts
  deriving (Eq, Generic, NFData, Show)


-- ** Edge data
------------------------------------------------------------------------------
data Lines
  = Lines
      { lineConnect :: !(DataArray Int)
      , lineOffsets :: !(DataArray Int)
      }
  | NoLines
  deriving (Eq, Generic, NFData, Show)


-- ** Polygon data
------------------------------------------------------------------------------
-- | Generic polygons.
data Polys
  = Polys
      { polyConnect :: !(DataArray Int)
      , polyOffsets :: !(DataArray Int)
      }
  | NoPolys
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | Triangle-strips.
data Strips
  = Strips
      { stripConnect :: !(DataArray Int)
      , stripOffsets :: !(DataArray Int)
      }
  | NoStrips
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | Currently just quadrants.
--
--   TODO:
--    + moar!
--
data Cells
  = Cells
      { connectivity :: !(DataArray Int)
      , cellOffsets  :: !(DataArray Int)
      , cellTypes    :: !(DataArray VtkCellType)
      }
  deriving (Eq, Generic, NFData, Show)

data CellData a
  = CellData
      { cscalars :: [DataArray a]
      , cvectors :: [DataArray a]
      , cnormals :: [DataArray a]
      , ctensors :: [DataArray a]
      , ctcoords :: [DataArray a]
      }
  deriving (Eq, Generic, NFData, Show)


-- * Instances
------------------------------------------------------------------------------
instance Semigroup (PointData t) where
  PointData ss vs ns ts uv <> PointData sz vz nz tz uz =
    PointData (ss <> sz) (vs <> vz) (ns <> nz) (ts <> tz) (uv <> uz)

instance Monoid (PointData t) where
  mempty = PointData [] [] [] [] []

------------------------------------------------------------------------------
instance Semigroup (CellData t) where
  CellData ss vs ns ts uv <> CellData sz vz nz tz uz =
    CellData (ss <> sz) (vs <> vz) (ns <> nz) (ts <> tz) (uv <> uz)

instance Monoid (CellData t) where
  mempty = CellData [] [] [] [] []


-- * Lenses
------------------------------------------------------------------------------
makeLenses ''DataArray
makeLenses ''Coordinates


-- * Smart constructors
------------------------------------------------------------------------------
noPointData :: PointData t
noPointData  = PointData [] [] [] [] []

noCellData  :: CellData t
noCellData   = CellData [] [] [] [] []


-- * Queries
------------------------------------------------------------------------------
{-- }
numPoints :: Points a -> Int
numPoints (PointsChunked (DataArray _ n 3 _ _)) = n
numPoints (PointsStriped (Coordinates (DataArray _ n 1 _ _) _ _)) = n
numPoints _ = error "Data.VTK.Unstructured.numPoints: internal error"

numCells :: Cells -> Int
numCells (Cells _ _ (DataArray _ n 1 _ _)) = n
numCells _ = error "Data.VTK.Unstructured.numCells: internal error"
--}
