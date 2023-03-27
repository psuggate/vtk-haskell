{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.VTK.DataArraySpec as Data
import           Data.VTK.Unstructured  as Unst
import qualified Data.VTK.XenoSpec      as Xeno
import           Test.Hspec
import           Test.VTK.Utils         (emptyVTU, filePath)

-- import qualified Data.VTK.Parser        as Mega


-- * Assorted tests
------------------------------------------------------------------------------
vtkTests :: Spec
vtkTests  = context "Top-level VTK tests" $ do

  describe "General-purpose I/O VTU tests" $ do

    it "can read an empty VTU file" $ do
      vtu <- Unst.readFileVTU filePath
      vtu `shouldBe` emptyVTU


-- * Top-level tests
------------------------------------------------------------------------------
tests :: Spec
tests  = do
  Data.spec
  Xeno.spec
  vtkTests

{-- }
  describe "MegaParsec-based VTU tests" $ do

    it "can read an empty VTU file" $ do
      vtk <- Mega.parseUnstructuredMesh filePath
      vtk `shouldBe` Just emptyVTK
--}


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  -- Setup data-files for the tests
  Unst.writeFileVTU filePath emptyVTU

  -- Run the tests
  hspec tests
