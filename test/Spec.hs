{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import qualified Data.VTK.Parser       as Mega
import           Data.VTK.Types
import           Data.VTK.Unstructured as Unst
import qualified Data.VTK.Xeno         as Xeno
import           Test.Hspec


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
               (DataArray "Float32" 0 1 "offsets" "")
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


-- * Top-level tests
------------------------------------------------------------------------------
tests :: Spec
tests  = context "VTK unstructured-grid tests" $ do

  describe "Xeno-based VTU tests" $ do

    it "can read an empty VTU file" $ do
      vtu <- VTU . Xeno.pieces <$> Xeno.parseUnstructuredMeshFile filePath
      vtu `shouldBe` emptyVTU

{-- }
  describe "MegaParsec-based VTU tests" $ do

    it "can read an empty VTU file" $ do
      vtk <- Mega.parseUnstructuredMesh filePath
      vtk `shouldBe` Just emptyVTK
--}

  describe "General-purpose I/O VTU tests" $ do

    it "can read an empty VTU file" $ do
      vtu <- Unst.readFileVTU filePath
      vtu `shouldBe` emptyVTU


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  -- Setup data-files for the tests
  Unst.writeFileVTU filePath emptyVTU

  -- Run the tests
  hspec tests
