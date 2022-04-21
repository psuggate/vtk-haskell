module Data.VTK.XenoSpec where

import           Data.VTK.Unstructured as Unst
import qualified Data.VTK.Xeno         as Xeno
import           Test.Hspec
import           Test.VTK.Utils


spec :: Spec
spec  = context "VTK unstructured-grid tests" $ do

  describe "Xeno-based VTU tests" $ do

    it "can read an empty VTU file" $ do
      vtu <- VTU . Xeno.pieces <$> Xeno.parseUnstructuredMeshFile filePath
      vtu `shouldBe` emptyVTU

    it "can read a square-mesh VTU file" $ do
      let fp = "unit-square.vtu"
      sq <- squareVTU
      Unst.writeFileVTU fp sq
      vtu <- VTU . Xeno.pieces <$> Xeno.parseUnstructuredMeshFile fp
      vtu `shouldBe` sq
