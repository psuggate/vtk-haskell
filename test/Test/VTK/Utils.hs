{-# LANGUAGE OverloadedStrings #-}

module Test.VTK.Utils where

import           Data.Int
import qualified Data.VTK.Parser       as Mega
import           Data.VTK.Types
import           Data.VTK.Unstructured as Unst
import qualified Data.Vector.Storable  as Vec


-- * Defaults
------------------------------------------------------------------------------
filePath :: FilePath
filePath  = "unstructured.vtu"

emptyVTU :: VTU
emptyVTU  = VTU [ Piece ps pd cs cd ]
  where
    ps = PointsStriped $ Coordinates dx dy dz
    dx = DataArray "Float32" 0 1 "xcoords" ""
    dy = DataArray "Float32" 0 1 "ycoords" ""
    dz = DataArray "Float32" 0 1 "zcoords" ""
    cs = Cells (DataArray "Int32" 0 1 "connectivity" "")
               (DataArray "Int32" 0 1 "offsets" "")
               (DataArray "UInt8" 0 1 "types" "")
    pd = PointData [] [] [] [] []
    cd = CellData [] [] [] [] []

emptyVTK :: Mega.VtkFile
emptyVTK  = Mega.VtkFile attrs [piece]
  where
    attrs = Mega.VtkAttrs Mega.VtkUnstructuredGrid "0.1" "vtkZLibDataCompressor" ()
    piece = Mega.VtkPiece Mega.VtkUnstructuredGrid verts cells
    verts = Mega.VtkPointsStriped mempty mempty mempty
    cells = Mega.VtkCells VtkQuad mempty

squareVTU :: IO VTU
squareVTU  = do
  dx <- encodeArray "xcoords" $ Vec.fromList [0.0, 1.0, 1.0, 0.0 :: Float]
  dy <- encodeArray "ycoords" $ Vec.fromList [0.0, 0.0, 1.0, 1.0 :: Float]
  dz <- encodeArray "zcoords" $ Vec.fromList [0.0, 0.0, 0.0, 0.0 :: Float]
  ci <- encodeArray "connectivity" $ Vec.fromList [0, 1, 2, 3 :: Int32]
  os <- encodeArray "offsets" $ Vec.fromList [4 :: Int32]
  ts <- encodeArray "types"   $ Vec.singleton VtkQuad

  let ps = PointsStriped $ Coordinates dx dy dz
      cs = Cells ci os ts
      pd = PointData [] [] [] [] []
      cd = CellData [] [] [] [] []

  pure $ VTU [ Piece ps pd cs cd ]
