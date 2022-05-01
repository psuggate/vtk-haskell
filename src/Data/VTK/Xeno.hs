{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia,
             GeneralisedNewtypeDeriving, OverloadedStrings #-}

module Data.VTK.Xeno
  (
    parseUnstructuredMesh
  , parseUnstructuredMeshFile
  , unstructuredGrid
  , VtkFile (..)
  )
where

import           Control.Arrow              ((***), (<<<), (|||))
import           Control.Monad              (unless, (>=>))
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Char                  as Char
import qualified Data.Text.Lazy.Encoding    as Text
import           Data.VTK.Core              as VTK
import           GHC.Generics               (Generic)
import           Text.Printf
import           UnliftIO.Exception
import           Xeno.DOM                   as Xeno


-- * VTK parser data-types
------------------------------------------------------------------------------
newtype VtkFile
  = VtkFile { pieces :: [Piece] }
  deriving (Eq, Generic, Monoid, Semigroup, Show)

newtype VtkParseException
  = VtkParseException { _showVtkParseException :: String }
  deriving (Eq, Generic, Show)

type XmlAttrs = [(BS.ByteString, BS.ByteString)]


-- * Lenses and instances
------------------------------------------------------------------------------
instance Exception VtkParseException


-- * Top-level parsers
------------------------------------------------------------------------------
parseUnstructuredMeshFile :: FilePath -> IO VtkFile
parseUnstructuredMeshFile  = BS.readFile >=> parseUnstructuredMesh

parseUnstructuredMesh :: BS.ByteString -> IO VtkFile
parseUnstructuredMesh bs = do
  vt <- case Xeno.parse bs of
    Left er -> throwIO er
    Right x -> pure x

  let label = BS.unpack $ name vt
  unless (label == "VTKFile") $ do
    print vt
    throwIO $ VtkParseException $ printf "unexpected top-level node: '%s'" label

  let attrs = attributes vt
  unless (checkAttrs attrs) $ do
    mapM_ (putStrLn . (uncurry $ printf "  %s: %s") <<< BS.unpack *** show) attrs
    throwIO $ VtkParseException $ printf "invalid attributes: %s" (show attrs)

  let nodes = children vt
      child = head nodes
  unless (length nodes == 1) $ do
    throwIO $ VtkParseException $ printf "only one mesh expected"

  -- print child

  case unstructuredGrid child of
    Left er -> throwIO er
    Right x -> pure $ VtkFile x

------------------------------------------------------------------------------
unstructuredGrid :: Xeno.Node -> Either VtkParseException [Piece]
unstructuredGrid node
  | label /= "UnstructuredGrid" = throwE $ printf "unexpected node: '%s'" label
  | not (null attrs)   = throwE $ printf "unexpected attributes:\n%s" attrs
  | null (textOf node) = sequence $ getPiece <$> children node
  | otherwise          = throwE $ printf "unexpected text/body:\n%s" text
  where
    label = BS.unpack $ name node
    attrs = unlines $ (uncurry fmt <<< BS.unpack *** show) <$> attributes node
    fmt   = printf "  %s: %s"
    text  = BS.unpack . BS.unlines $ textOf node


-- ** Piece-parsers
------------------------------------------------------------------------------
getPiece :: Xeno.Node -> Either VtkParseException Piece
getPiece node
  | label == "Piece" = Piece <$> ps <*> pd <*> cs <*> cd
  | otherwise = throwE "invalid 'Piece'"
  where
    label = name node
    nodes = children node
    ps = node `itsOnly` "Points" >>= getPoints
    -- ps = element "Points" getPoints nodes
    pd = optional "PointData" (Right mempty) getPointData nodes
    cs = node `itsOnly` "Cells" >>= getCells
    -- cs = element "Cells" getCells nodes
    cd = optional "CellData" (Right mempty) getCellData nodes


-- ** Points parsers
------------------------------------------------------------------------------
getPoints :: Xeno.Node -> Either VtkParseException Points
getPoints node
  | name node == "Points"
  , null (attributes node)
  , length nodes == 1
  , null (textOf node) = getPointsOf (head nodes)
  | otherwise = throwE "invalid 'Points'"
  where
    nodes = children node

getPointsOf :: Xeno.Node -> Either VtkParseException Points
getPointsOf node
  | null (textOf node) = case name node of
      "Coordinates" -> if null attrs && length nodes == 3
        then PointsStriped <$> getCoords' node
        else throwE "invalid (striped) 'Coordinates'"
      "DataArray" -> if length attrs >= 3
        then PointsChunked <$> getDataArray node
        else throwE "invalid (chunked) 'Points'"
      _ -> throwE . printf "unexpected node '%s'" . BS.unpack $ name node
  | otherwise = throwE "invalid 'Points' element"
  where
    nodes = children node
    attrs = attributes node

getCoords' :: Xeno.Node -> Either VtkParseException Coordinates
getCoords' node = Coordinates <$> getDataArrayNamed "xcoords" nodes
                              <*> getDataArrayNamed "ycoords" nodes
                              <*> getDataArrayNamed "zcoords" nodes
  where
    nodes = children node


-- ** Parsers for point-data
------------------------------------------------------------------------------
getPointData :: Xeno.Node -> Either VtkParseException PointData
getPointData  = const (pure mempty)
{-# WARNING getPointData "STUB" #-}


-- ** Cells parsers
------------------------------------------------------------------------------
getCells :: Xeno.Node -> Either VtkParseException Cells
getCells node = Cells
                <$> getDataArrayNamed "connectivity" nodes
                <*> getDataArrayNamed "offsets" nodes
                <*> getDataArrayNamed "types" nodes
  where
    nodes = children node


-- ** Parsers for cell-data
------------------------------------------------------------------------------
getCellData :: Xeno.Node -> Either VtkParseException CellData
getCellData  = const (pure mempty)
{-# WARNING getCellData "STUB" #-}


-- * Building-blocks parsers
------------------------------------------------------------------------------
{-- }
element
  :: BS.ByteString
  -> (Xeno.Node -> Either VtkParseException a)
  -> [Xeno.Node]
  -> Either VtkParseException a
element label = label `optional` throwE ("Cannot find element " ++ show label)
--}

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


-- ** Parsers for @DataArray@'s
------------------------------------------------------------------------------
-- | Build a @DataArray@ from the given XML node.
--
--   TODO:
--    + support for the 'NumberOfComponents' attribute;
--
getDataArray :: Xeno.Node -> Either VtkParseException DataArray
getDataArray node
  | name node == "DataArray"
  , length attrs >= 3 = fmap setArrayProps $ DataArray
                        <$> ltext `fmap` getAttribute "type" attrs
                        <*> pure (-1)
                        <*> getNumberOfComponents attrs
                        <*> ltext `fmap` getAttribute "Name" attrs
                        <*> (pure . ltext . BS.concat . textOf) node
  | otherwise = throwE "invalid 'DataArray'"
  where
    ltext = Text.decodeUtf8 . BL.fromStrict
    attrs = attributes node

getNumberOfComponents :: XmlAttrs -> Either VtkParseException Int
getNumberOfComponents  =
  Right . (const 1 ||| read . BS.unpack) . getAttribute "NumberOfComponents"

getDataArrayNamed
  :: BS.ByteString
  -> [Xeno.Node]
  -> Either VtkParseException DataArray
getDataArrayNamed label nodes
  | length ns == 1 = getDataArray $ head ns
  | null ns        = throwE $ printf "'DataArray' named '%s' not found" l'
  | otherwise      = throwE $ printf "expected just one '%s' array" l'
  where
    ns = filter (\n ->
                    name n == "DataArray" &&
                    hasAttributeValue "Name" label n
                ) nodes
    l' = BS.unpack label


-- ** Attribute queries and parsers
------------------------------------------------------------------------------
getAttribute :: BS.ByteString -> XmlAttrs -> Either VtkParseException BS.ByteString
getAttribute key = maybe err Right . lookup key where
  err = throwE $ printf "attribute '%s' not found" $ BS.unpack key

hasAttributeValue :: BS.ByteString -> BS.ByteString -> Xeno.Node -> Bool
hasAttributeValue k v = maybe False (== v) . lookup k . attributes

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
-- TODO: should only strip leading and trailing whitespace?
textOf :: Xeno.Node -> [BS.ByteString]
textOf  = go . contents where
  go         []  = []
  go (Text t:ts)
    | BS.null t' = go ts
    | otherwise  = t':go ts
    where t'     = BS.filter (not . Char.isSpace) t
  go (     _:ts) = go ts

-- allSpaces :: BS.ByteString -> Bool
-- allSpaces  = BS.all Char.isSpace

-- ** Queries & exceptions
------------------------------------------------------------------------------
throwE :: String -> Either VtkParseException a
throwE  = Left . VtkParseException

itsOnly :: Xeno.Node -> BS.ByteString -> Either VtkParseException Xeno.Node
itsOnly node label
  | null found       = throwE $ printf "node '%s' not found as child of '%s'" label' parent
  | length found > 1 = throwE $ printf "too many child nodes of '%s' with name '%s'" parent label'
  | otherwise        = Right $ head found
  where
    found  = ((== label) . name) `filter` children node
    label' = BS.unpack label
    parent = BS.unpack (name node)
