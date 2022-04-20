{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances, GADTs,
             LambdaCase, OverloadedStrings, PatternSynonyms,
             ScopedTypeVariables, TemplateHaskell, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.Core
-- Copyright   : (C) Patrick Suggate, 2021
-- License     : BSD3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
-- Parser for VTK files.
--
-- == Changelog
--  - 08/07/2021  --  initial file;
--
-- == TODO
--  - move most of this stuff to 'Data.VTK.Emit'?
--
------------------------------------------------------------------------------

module Data.VTK.Core
  (
    VtkDoc (..)

  , Piece (..)
  , Points (..)
  , Coordinates (..)
  , PointData (..)
  , Cells (..)
  , CellData (..)

  , noPointData
  , noCellData

  , module Data.VTK.DataArray
  , module Data.VTK.Types
  )
where

import           Control.DeepSeq              (NFData)
import qualified Data.Text.Lazy               as L
import           Data.VTK.DataArray
import           Data.VTK.Types
import           GHC.Generics                 (Generic)
import           Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as P


-- * Serialisation type-classes
------------------------------------------------------------------------------
class VtkDoc a where
  dshow :: a -> Doc


-- * VTK data types
------------------------------------------------------------------------------
-- | Mesh substructures.
data Piece
  = Piece !Points !PointData !Cells !CellData
  deriving (Eq, Generic, NFData, Show)


-- ** Vertex data
------------------------------------------------------------------------------
data Points
  = PointsChunked !DataArray
  | PointsStriped !Coordinates
  deriving (Eq, Generic, NFData, Show)

------------------------------------------------------------------------------
-- | Coordinates of vertices ('Points') are required to have three (spatial)
--   components.
data Coordinates
  = Coordinates
      { xcoords :: !DataArray
      , ycoords :: !DataArray
      , zcoords :: !DataArray
      }
  deriving (Eq, Generic, NFData, Show)

data PointData
  = PointData
      { pscalars :: [DataArray]
      , pvectors :: [DataArray]
      , pnormals :: [DataArray]
      , ptensors :: [DataArray]
      , ptcoords :: [DataArray]
      }
  deriving (Eq, Generic, NFData, Show)


-- ** Polygon data
------------------------------------------------------------------------------
-- | Currently just quadrants.
data Cells
  = Cells
      { connectivity :: !DataArray
      , cellOffsets  :: !DataArray
      , cellTypes    :: !DataArray
      }
  deriving (Eq, Generic, NFData, Show)

data CellData
  = CellData
      { cscalars :: [DataArray]
      , cvectors :: [DataArray]
      , cnormals :: [DataArray]
      , ctensors :: [DataArray]
      , ctcoords :: [DataArray]
      }
  deriving (Eq, Generic, NFData, Show)


-- * Instances
------------------------------------------------------------------------------
instance Semigroup PointData where
  PointData ss vs ns ts uv <> PointData sz vz nz tz uz =
    PointData (ss <> sz) (vs <> vz) (ns <> nz) (ts <> tz) (uv <> uz)

instance Monoid PointData where
  mempty = PointData [] [] [] [] []

------------------------------------------------------------------------------
instance Semigroup CellData where
  CellData ss vs ns ts uv <> CellData sz vz nz tz uz =
    CellData (ss <> sz) (vs <> vz) (ns <> nz) (ts <> tz) (uv <> uz)

instance Monoid CellData where
  mempty = CellData [] [] [] [] []


-- ** Pretty-printing instances
------------------------------------------------------------------------------
instance VtkDoc Cells where
  dshow (Cells co os ty) = P.nest 2 ("<Cells>" P.<$>
                                     dshow co  P.<$>
                                     dshow os  P.<$>
                                     dshow ty) P.<$> "</Cells>"

instance VtkDoc CellData where
  dshow (CellData ss vs _ _ _)
    | null ss
    , null vs   = mempty
    | otherwise = P.nest 2 (cod P.<$> dat) P.<$> "</CellData>"
    where
      cod = "<CellData " <> go "Scalars=" ss <> go "Vectors=" vs <> ">"
      go l xs
        | null xs   = mempty
        | otherwise = l <> names xs
      dat = P.vsep . map dshow $ ss ++ vs

------------------------------------------------------------------------------
instance VtkDoc Points where
  dshow ps = P.nest 2 ("<Points>" P.<$> xs) P.<$> "</Points>" where
    xs = case ps of
      PointsChunked da -> dshow da
      PointsStriped cs -> dshow cs

instance VtkDoc Coordinates where
  dshow (Coordinates dx dy dz) =
    let xs = P.vsep $ map dshow [dx, dy, dz]
    in  P.nest 2 ("<Coordinates>" P.<$> xs) P.<$> "</Coordinates>"

instance VtkDoc PointData where
  dshow (PointData ss vs _ _ _)
    | null ss
    , null vs   = mempty
    | otherwise = P.nest 2 (pod P.<$> dat) P.<$> "</PointData>"
    where
      pod = "<PointData " <> go "Scalars=" ss <> go "Vectors=" vs <> ">"
      go l xs
        | null xs   = mempty
        | otherwise = l <> names xs
      dat = P.vsep . map dshow $ ss ++ vs

------------------------------------------------------------------------------
-- | Pretty-printing of @DataArray@'s.
instance VtkDoc DataArray where
  dshow (DataArray t _ d l xs) =
    let typ = "type=" <> P.dquotes (P.text t) <> P.char ' '
        noc | d   == 1  = mempty :: Doc
            | otherwise = "NumberOfComponents=" <> quotes d <> P.char ' '
        nam = "Name=" <> P.dquotes (P.text l) <> P.char ' ' :: Doc
        atr = typ <> nam <> noc <> "format=\"binary\""
        hdr = P.angles $ "DataArray " <> atr :: Doc
    in  P.nest 2 (hdr P.<$> P.text xs) P.<$> "</DataArray>"

------------------------------------------------------------------------------
-- | Pretty-print pieces of a VTU.
instance VtkDoc Piece where
  dshow (Piece ps pd cs cd) =
    let px = P.angles $ "Piece " <> np <> P.char ' ' <> nc
        np = "NumberOfPoints=" <> quotes (numPoints ps)
        nc = "NumberOfCells=" <> quotes (numCells cs)
    in  P.nest 2 (px        P.<$>
                  dshow ps  P.<$>
                  dshow pd  P.<$>
                  dshow cs  P.<$>
                  dshow cd) P.<$> "</Piece>"


-- * Smart constructors
------------------------------------------------------------------------------
noPointData :: PointData
noPointData  = PointData [] [] [] [] []

noCellData  :: CellData
noCellData   = CellData [] [] [] [] []


-- * Queries
------------------------------------------------------------------------------
numPoints :: Points -> Int
numPoints (PointsChunked (DataArray _ n 3 _ _)) = n
numPoints (PointsStriped (Coordinates (DataArray _ n 1 _ _) _ _)) = n
numPoints _ = error "Data.VTK.Unstructured.numPoints: internal error"

numCells :: Cells -> Int
numCells (Cells _ _ (DataArray _ n 1 _ _)) = n
numCells _ = error "Data.VTK.Unstructured.numCells: internal error"


-- * Helper functions
------------------------------------------------------------------------------
quotes :: Show a => a -> Doc
quotes  = P.dquotes . P.text . L.pack . show
{-# INLINE quotes #-}

names :: [DataArray] -> Doc
names  = P.dquotes . P.text . L.intercalate "," . map arrayName
