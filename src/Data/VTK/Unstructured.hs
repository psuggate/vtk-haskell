{-# LANGUAGE FlexibleInstances, GADTs, LambdaCase, OverloadedStrings,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.Unstructured
-- Copyright   : (C) Patrick Suggate, 2020
-- License     : BSD3
--
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : Experimental
-- Portability : non-portable
--
-- Representations and functions for VTK, unstructured meshes.
--
-- == Changelog
--  - 23/10/2020  --  initial file;
--
------------------------------------------------------------------------------

module Data.VTK.Unstructured
  (
    -- Re-exports
    module Data.VTK.Types

    -- Type classes
  , VtkDoc (..)
  , VtkArray (..)

    -- Data types
  , VTU (..)
  , Piece (..)
  , Points (..)
  , Coordinates (..)
  , Cells (..)
  , PointData (..)
  , CellData (..)
  , DataArray (..)

    -- I/O
  , renderToByteString
  , writeFileVTU
  , readFileVTU

    -- Conversions
  , fromVector

    -- Helpers
  , noPointData
  , noCellData
  )
where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Lazy.Encoding      as E
import           Data.VTK.DataArray
import           Data.VTK.Types
import           Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as P


-- * Serialisation type-classes
------------------------------------------------------------------------------
class VtkDoc a where
  dshow :: a -> Doc


-- * VTK data types
------------------------------------------------------------------------------
-- | Top-level, unstructured-mesh data type.
newtype VTU = VTU [Piece]

-- | Mesh substructures.
data Piece = Piece !Points !PointData !Cells !CellData


-- ** Vertex data
------------------------------------------------------------------------------
data Points
  = PointsChunked !DataArray
  | PointsStriped !Coordinates

------------------------------------------------------------------------------
-- | Coordinates of vertices ('Points') are required to have three (spatial)
--   components.
data Coordinates
  = Coordinates
      { xcoords :: !DataArray
      , ycoords :: !DataArray
      , zcoords :: !DataArray
      }

data PointData
  = PointData
      { pscalars :: [DataArray]
      , pvectors :: [DataArray]
      , pnormals :: [DataArray]
      , ptensors :: [DataArray]
      , ptcoords :: [DataArray]
      }


-- ** Polygon data
------------------------------------------------------------------------------
-- | Currently just quadrants.
data Cells
  = Cells
      { connectivity :: !DataArray
      , cellOffsets  :: !DataArray
      , cellTypes    :: !DataArray
      }

data CellData
  = CellData
      { cscalars :: [DataArray]
      , cvectors :: [DataArray]
      , cnormals :: [DataArray]
      , ctensors :: [DataArray]
      , ctcoords :: [DataArray]
      }


-- * Instances
------------------------------------------------------------------------------
instance Show VTU where
  show = show . dshow


-- ** Pretty-printing instances
------------------------------------------------------------------------------
instance VtkDoc VTU where
  dshow (VTU ps) = vtkfile doc where
    doc = P.nest 2 ("<UnstructuredGrid>" P.<$> pcs
                   ) P.<$> "</UnstructuredGrid>"
    pcs = P.vsep (map dshow ps)

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
instance VtkDoc DataArray where
  dshow (DataArray t _ d l xs) =
    let typ = "type=" <> P.dquotes (P.text t) <> P.char ' '
        noc | d   == 1  = mempty :: Doc
            | otherwise = "NumberOfComponents=" <> quotes d <> P.char ' '
        nam = "Name=" <> P.dquotes (P.text l) <> P.char ' ' :: Doc
        atr = typ <> nam <> noc <> "format=\"binary\""
        hdr = P.angles $ "DataArray " <> atr :: Doc
    in  P.nest 2 (hdr P.<$> P.text xs) P.<$> "</DataArray>"


-- * VTK encoding helpers
------------------------------------------------------------------------------
version :: Doc
version  = "<?xml version=\"1.0\"?>"
{-# INLINE[2] version #-}

vtkfile :: Doc -> Doc
vtkfile doc = version P.<$> P.nest 2 (hdr P.<$> doc) P.<$> "</VTKFile>" where
  hdr = "<VTKFile " <> typ <> ver <> cmp <> b_o
  typ = "type=\"UnstructuredGrid\" "
  ver = "version=\"0.1\" "
  cmp = "compressor=\"vtkZLibDataCompressor\" "
  b_o = "byte_order=\"LittleEndian\">"
{-# INLINE[2] vtkfile #-}


-- ** Smart constructors
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


-- * I/O
------------------------------------------------------------------------------
renderToByteString :: Doc -> BL.ByteString
-- renderToByteString  = E.encodeUtf8 . P.displayT . P.renderOneLine
renderToByteString  = E.encodeUtf8 . P.displayT . P.renderPretty 1.0 maxBound

writeFileVTU :: MonadIO m => FilePath -> VTU -> m ()
writeFileVTU fp = liftIO . BL.writeFile fp . renderToByteString . dshow

readFileVTU :: MonadIO m => FilePath -> m VTU
readFileVTU fp = liftIO $ do
  -- readPieceVTU
  let ps = []
  pure $ VTU ps


-- * Helper functions
------------------------------------------------------------------------------
quotes :: Show a => a -> Doc
quotes  = P.dquotes . P.text . L.pack . show
{-# INLINE quotes #-}

names :: [DataArray] -> Doc
names  = P.dquotes . P.text . L.intercalate "," . map arrayName
