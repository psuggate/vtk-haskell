{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia,
             GeneralisedNewtypeDeriving, OverloadedStrings #-}
module Data.VTK.Xeno
  (
    parseUnstructuredMesh
  , unstructuredGrid
  , VtkFile (..)
  )
where

import           Control.Applicative   (Alternative)
import           Control.Monad         (unless)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char             as Char
import           GHC.Generics          (Generic)
import           Text.Printf
import           UnliftIO.Exception
import           Xeno.DOM              as Xeno

import           Data.VTK.Core         as VTK


-- * VTK parser data-types
------------------------------------------------------------------------------
newtype VtkFile
  = VtkFile { pieces :: [Piece] }
  deriving (Monoid, Semigroup)

newtype VtkParseException
  = VtkParseException { _showVtkParseException :: String }
  deriving (Eq, Generic, Show)

type XmlAttrs = [(BS.ByteString, BS.ByteString)]


-- * Lenses and instances
------------------------------------------------------------------------------
instance Exception VtkParseException


-- * Top-level parsers
------------------------------------------------------------------------------
parseUnstructuredMesh :: FilePath -> IO (Maybe VtkFile)
parseUnstructuredMesh file = do
  bs <- BS.readFile file
  case Xeno.parse bs of
    -- Left er -> throwIO $ VtkParseException $ displayException er
    Left er -> throwIO er
    Right x -> do
      print x
      let attrs = attributes x
      unless (checkAttrs attrs) $ do
        throwIO $ VtkParseException $ printf "invalid attributes: %s" (show attrs)
--       _ <- unstructuredGrid x
      pure Nothing

------------------------------------------------------------------------------
unstructuredGrid :: Xeno.Node -> Either VtkParseException [Piece]
unstructuredGrid node
  | name node == "UnstructuredGrid"
  , null (attributes node)
  , null (textOf node) = sequence $ getPiece <$> children node
  | otherwise          = throwE "invalid 'UnstructuredGrid'"

{-- }
unstructuredGrid' :: [Xeno.Content] -> Either VtkParseException [Piece]
unstructuredGrid' content
  |

  , null (attributes node)
  , null (textOf node) = sequence $ getPiece <$> children node
  | otherwise          = throwE "invalid 'UnstructuredGrid'"
--}


-- ** Piece-parsers
------------------------------------------------------------------------------
getPiece :: Xeno.Node -> Either VtkParseException Piece
getPiece node
  | label == "Piece" = Piece <$> ps <*> pd <*> cs <*> cd
  | otherwise = throwE "invalid 'Piece'"
  where
    label = name node
    nodes = children node
    ps = element "Points" getPoints nodes
    pd = optional "PointData" (Right mempty) getPointData nodes
    cs = element "Cells" getCells nodes
    cd = optional "CellData" (Right mempty) getCellData nodes


-- ** Points parsers
------------------------------------------------------------------------------
getPoints :: Xeno.Node -> Either VtkParseException Points
getPoints node
  | name node == "Points"
  , null (attributes node)
  , length nodes == 1
  , null (textOf node) = getPoints' (head nodes)
  | otherwise = throwE "invalid 'Points'"
  where
    nodes = children node

getPoints' :: Xeno.Node -> Either VtkParseException Points
getPoints' node
  | length nodes == 1
  , null (textOf node) = case name node of
      "Coordinates" -> if null attrs
        then PointsStriped <$> getCoords' (contents node)
        else throwE "invalid (striped) 'Coordinates'"
      "DataArray" -> if length attrs >= 3
        then PointsChunked <$> getDataArray node
        else throwE "invalid (chunked) 'Points'"
  | otherwise = throwE "invalid 'Points' element"
  where
    nodes = children node
    attrs = attributes node

getCoords' :: [Xeno.Content] -> Either VtkParseException Coordinates
getCoords'  = undefined


-- ** Parsers for point-data
------------------------------------------------------------------------------
getPointData :: Xeno.Node -> Either VtkParseException PointData
getPointData  = undefined


-- ** Cells parsers
------------------------------------------------------------------------------
getCells :: Xeno.Node -> Either VtkParseException Cells
getCells  = undefined


-- ** Parsers for cell-data
------------------------------------------------------------------------------
getCellData :: Xeno.Node -> Either VtkParseException CellData
getCellData  = undefined


-- * Building-blocks parsers
------------------------------------------------------------------------------
element
  :: BS.ByteString
  -> (Xeno.Node -> Either VtkParseException a)
  -> [Xeno.Node]
  -> Either VtkParseException a
element label = label `optional` throwE ("Cannot find element " ++ show label)

optional
  :: BS.ByteString
  -> Either VtkParseException a
  -> (Xeno.Node -> Either VtkParseException a)
  -> [Xeno.Node]
  -> Either VtkParseException a
optional label missing parser = go where
  go    []            = missing
  go (n:ns)
    | name n == label = parser n
    | otherwise       = go ns

itsOnly :: Xeno.Node -> BS.ByteString -> Either VtkParseException Xeno.Node
itsOnly node label
  | null found       = throwE $ printf "node '%s' not found as child of '%s'" label' parent
  | length found > 1 = throwE $ printf "too many child nodes of '%s' with name '%s'" parent label'
  | otherwise        = Right $ head found
  where
    found  = ((== label) . name) `filter` children node
    label' = BS.unpack label
    parent = BS.unpack (name node)

getDataArray :: Xeno.Node -> Either VtkParseException DataArray
getDataArray node
  | name node == "DataArray"
  , length attrs >= 3 = undefined
  | otherwise = throwE "invalid 'DataArray'"
  where
    attrs = attributes node


{-- }
-- * Convert XML into VTK representation
------------------------------------------------------------------------------
-- | Check and process the contents of the XML file.
xmlToVtk :: Node -> VtkFile
xmlToVtk node
  | label=="VTKFile"
  , checkAttrs attrs = VtkFile $ go nodes
  | otherwise        = mempty
  where
    label = name node
    attrs = attributes node
    nodes = children node

parsePiece :: Node -> Either String Piece
parsePiece node
  | label=="Piece" = Right $ Piece ps pd cs cd
  | otherwise
--}

checkAttrs :: [(BS.ByteString, BS.ByteString)] -> Bool
checkAttrs         []  = True
checkAttrs ((k, v):xs) = case k of
  "type"       -> if v == "UnstructuredGrid" then checkAttrs xs else False
  "compressor" -> if v == "vtkZLibDataCompressor" then checkAttrs xs else False
  "byte_order" -> if v == "LittleEndian" then checkAttrs xs else False
  "version"    -> if v == "0.1" then checkAttrs xs else False
  _            -> False


-- * Helpers
------------------------------------------------------------------------------
textOf :: Xeno.Node -> [Xeno.Content]
textOf  = filter go . contents where
  go (Text ts) = not (allSpaces ts)
  go _         = False

{--}
dropSpaces :: [Xeno.Content] -> [Xeno.Content]
dropSpaces (Text ts:cs)
  | allSpaces ts = dropSpaces cs
  | otherwise    = undefined
--}

allSpaces :: BS.ByteString -> Bool
allSpaces  = BS.all Char.isSpace

throwE :: String -> Either VtkParseException a
throwE  = Left . VtkParseException
