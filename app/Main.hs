{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.VTK.Unstructured
import           Data.VTK.Xeno


-- * Defaults
------------------------------------------------------------------------------
filePath :: FilePath
filePath  = "unstructured.vtu"


-- * Testing & examples
------------------------------------------------------------------------------
testUnstructured :: IO ()
testUnstructured  = do
  let mesh = VTU [ Piece ps pd cs cd
                 ]
      ps = PointsStriped $ Coordinates dx dy dz
      dx = DataArray "Float32" 0 1 "xcoords" ""
      dy = DataArray "Float32" 0 1 "ycoords" ""
      dz = DataArray "Float32" 0 1 "zcoords" ""
      cs = Cells (DataArray "Int32" 0 1 "connectivity" "")
                 (DataArray "Float32" 0 1 "offsets" "")
                 (DataArray "UInt8" 0 1 "types" "")
      pd = PointData [] [] [] [] []
      cd = CellData [] [] [] [] []

  -- putStrLn $ show mesh
  writeFileVTU filePath mesh


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  testUnstructured
  () <$ parseUnstructuredMesh filePath
