{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs,
             GeneralisedNewtypeDeriving, LambdaCase, OverloadedStrings,
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
    module Data.VTK.Core

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
  )
where

import           Control.DeepSeq              (NFData)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text.Lazy.Encoding      as E
import           Data.VTK.Core
import qualified Data.VTK.Xeno                as Xeno
import           GHC.Generics                 (Generic)
import           Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as P


-- * VTK data types
------------------------------------------------------------------------------
-- | Top-level, unstructured-mesh data type.
--
--   TODO:
--    + non-default attributes;
--    + proper support for multiple pieces;
--    + 3D "meshes";
--
newtype VTU
  = VTU [Piece]
  deriving (Eq, Generic, NFData)


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


-- * VTK encoding helpers
------------------------------------------------------------------------------
version :: Doc
version  = "<?xml version=\"1.0\"?>"

vtkfile :: Doc -> Doc
vtkfile doc = version P.<$> P.nest 2 (hdr P.<$> doc) P.<$> "</VTKFile>" where
  hdr = "<VTKFile " <> typ <> ver <> cmp <> b_o
  typ = "type=\"UnstructuredGrid\" "
  ver = "version=\"0.1\" "
  cmp = "compressor=\"vtkZLibDataCompressor\" "
  b_o = "byte_order=\"LittleEndian\">"


-- * I/O
------------------------------------------------------------------------------
renderToByteString :: Doc -> BL.ByteString
-- renderToByteString  = E.encodeUtf8 . P.displayT . P.renderOneLine
renderToByteString  = E.encodeUtf8 . P.displayT . P.renderPretty 1.0 maxBound

------------------------------------------------------------------------------
writeFileVTU :: MonadIO m => FilePath -> VTU -> m ()
writeFileVTU fp = liftIO . BL.writeFile fp . renderToByteString . dshow

readFileVTU :: MonadIO m => FilePath -> m VTU
readFileVTU fp = liftIO $ do
  VTU . Xeno.pieces <$> Xeno.parseUnstructuredMeshFile fp
