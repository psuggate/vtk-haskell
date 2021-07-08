{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs, LambdaCase,
             OverloadedStrings, PatternSynonyms, ScopedTypeVariables,
             TemplateHaskell, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.VTK.Parser
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
--  - 07/07/2021  --  initial file;
--
------------------------------------------------------------------------------

module Data.VTK.Parser (
    Parser
  , ParserErrorBundle

  , VtkFile (..)
  , vtkAttrs
  , vtkPieces

  , VtkType (..)

  , VtkAttrs (..)
  , vtkType
  , vtkVersion
  , vtkCompressor
  , vtkByteOrder

  , VtkPiece (..)
  , pieceType
  , piecePoints
  , pieceCells

  , VtkPoints (..)
  , _VtkPointsChunked
  , _VtkPointsStriped
  , VtkCells (..)

  , parseUnstructuredMesh
  )
where

import           Control.Lens               (makeLenses, makePrisms, set, (^.))
-- import           Data.Scientific
import qualified Data.Char                  as Char
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Data.Text.Lazy             as Lazy
import           Data.VTK.DataArray         (toVector)
import           Data.VTK.Types             (VtkCellType (..), pattern VtkQuad)
import           Data.Vector.Storable       (Storable, Vector)
-- import qualified Data.Vector.Storable       as Vec
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Linear                     (V3)
import           Text.Megaparsec            as Mega
import           Text.Megaparsec.Char       as Mega
import qualified Text.Megaparsec.Char.Lexer as Lex


-- * Defaults
------------------------------------------------------------------------------
type Parser = Parsec Void Text
type ParserErrorBundle = ParseErrorBundle Text Void

-- ** Convenience aliases (not exported)
------------------------------------------------------------------------------
type R = Double
type Z = Int


-- * VTK parser data-types
------------------------------------------------------------------------------
data VtkFile
  = VtkFile
      { _vtkAttrs  :: VtkAttrs
      , _vtkPieces :: [VtkPiece]
      }
  deriving (Eq, Generic, Show)

data VtkType
  = VtkUnstructuredGrid
  | VtkTypeUnknown
  deriving (Enum, Eq, Generic, Ord, Show)

data VtkAttrs
  = VtkAttrs
      { _vtkType       :: VtkType
      , _vtkVersion    :: Text
      , _vtkCompressor :: Text
      , _vtkByteOrder  :: ()
      }
  deriving (Eq, Generic, Show)

data VtkPiece
  = VtkPiece
      { _pieceType   :: VtkType
      , _piecePoints :: VtkPoints
      , _pieceCells  :: VtkCells
      }
  deriving (Eq, Generic, Show)

data VtkPoints
  = VtkPointsChunked (Vector (V3 R))
  | VtkPointsStriped (Vector R) (Vector R) (Vector R)
  deriving (Eq, Generic, Show)

data VtkCells
  = VtkCells VtkCellType (Vector Z)
  deriving (Eq, Generic, Show)

------------------------------------------------------------------------------
-- | For representing generic XML metadata.
type TagAttrs = [(Text, Text)]


-- * Lenses and instances
------------------------------------------------------------------------------
makeLenses ''VtkAttrs
makeLenses ''VtkFile
makeLenses ''VtkPiece

makePrisms ''VtkPoints
makePrisms ''VtkType


-- ** Some standard instances
------------------------------------------------------------------------------
-- | Left-associative combine.
instance Semigroup VtkAttrs where
  VtkAttrs t v c () <> x = VtkAttrs t' v' c' () where
    t' = case t of
      VtkTypeUnknown -> x ^. vtkType
      _              -> t
    v' = case v of
      "" -> x ^. vtkVersion
      _  -> v
    c' = case c of
      "" -> x ^. vtkCompressor
      _  -> c

instance Monoid VtkAttrs where
  mempty = VtkAttrs VtkTypeUnknown "" "" ()
  {-# INLINE mempty #-}


-- * Top-level parsers
------------------------------------------------------------------------------
parseUnstructuredMesh :: FilePath -> IO (Maybe VtkFile)
parseUnstructuredMesh file = do
  ts <- Text.readFile file
  let re = Mega.runParser (fn <* Mega.eof) file ts
      fn = VtkFile <$> (xmlmeta *> getattrs) <*> getbody VtkUnstructuredGrid
  case re of
    Left er -> putStrLn (Mega.errorBundlePretty er) *> pure Nothing
    Right x -> pure $ Just x


-- * More parsers
------------------------------------------------------------------------------
xmlmeta :: Parser ()
xmlmeta  = lexeme_ "<?xml" *> string "version=\"1.0\"" *> space <* string "?>"

getattrs :: Parser VtkAttrs
getattrs  = char '<' *> space *> lexeme_ "VTKFile" *> go mempty <* char '>' where
  go :: VtkAttrs -> Parser VtkAttrs
  go x = Mega.choice
    [ string "type=" *> (set vtkType `flip` x <$> dquoted gettype)
    , string "version=" *> (set vtkVersion `flip` x <$> dquoted version)
    , string "compressor=" *> cmp x
    , string "byte_order=" *> byt x
    ]
  cmp :: VtkAttrs -> Parser VtkAttrs
  cmp x = set vtkCompressor `flip` x <$> dquoted (string "vtkZLibDataCompressor")
  byt :: VtkAttrs -> Parser VtkAttrs
  byt x = x <$ dquoted (string "LittleEndian")

gettype :: Parser VtkType
gettype  = Mega.choice [ VtkUnstructuredGrid <$ string "UnstructuredGrid" ]

getbody :: VtkType -> Parser [VtkPiece]
getbody typ = snd <$> tagged tag go where
  tag = Text.pack . drop 3 $ show typ
  go :: Parser [VtkPiece]
  go  = Mega.some $ getpiece typ


-- ** Parse pieces
------------------------------------------------------------------------------
-- todo: better handling of unsupported mesh types
getpiece :: VtkType -> Parser VtkPiece
getpiece VtkUnstructuredGrid = snd <$> tagged "Piece" go where
  go :: Parser VtkPiece
  go  = VtkPiece VtkUnstructuredGrid <$> getpoints <*> getcells
getpiece t = error $ "unsupported VTK mesh type: " ++ show t

getpoints :: Parser VtkPoints
getpoints  = VtkPointsChunked . snd <$> tagged "Points" getcoords

getcells :: Parser VtkCells
getcells  = VtkCells VtkQuad . snd <$> tagged "Cells" getarray


-- ** Parse arrays
------------------------------------------------------------------------------
getarray :: forall a. Storable a => Parser (Vector a)
getarray  = toVector . Lazy.fromStrict <$> Mega.takeWhile1P msg b64 where
  msg :: Maybe String
  msg  = Just "base64 character"
  b64 :: Char -> Bool
  b64 c = Char.isLetter c || Char.isDigit c || c == '+' || c == '/' || c == '='

getcoords :: Parser (Vector (V3 R))
getcoords  = snd <$> tagged "Coordinates" go where
  go = snd <$> tagged "DataArray" getarray


-- * Helper parsers
------------------------------------------------------------------------------
dquoted :: Parser a -> Parser a
dquoted  = Mega.between (char '"') (char '"')

clabel :: Parser Text
clabel  = Text.pack <$> go where
  go = (:) <$> letterChar <*> Mega.many (choice [ alphaNumChar, char '_' ])

rvalue :: Parser Text
rvalue  = Text.pack <$> dquoted (Mega.many go) where
  go :: Parser Char
  go  = choice [ alphaNumChar, char '.', char '-', char '_', char ':', char ';' ]


-- ** General-purpose XML parsers
------------------------------------------------------------------------------
-- | Parse a (XML-)tagged section of the source-file.
tagged :: Text -> Parser a -> Parser (TagAttrs, a)
tagged tag p = (,) <$> xmlopen tag <*> (p <* xmlclose tag)

-- todo: how to handle metadata?
xmlopen :: Text -> Parser TagAttrs
xmlopen tag = try (char_ '<' *> lexeme_ tag *> tagattrs <* char_ '>')

tagattrs :: Parser TagAttrs
tagattrs  = Mega.many $ Lex.lexeme sc go where
  go = (,) <$> try (clabel <* char '=') <*> rvalue

xmlclose :: Text -> Parser ()
xmlclose tag = try (lexeme_ "</" *> lexeme_ tag *> char_ '>')


-- * Lexers
------------------------------------------------------------------------------
version :: Parser Text
version  = Text.pack <$> Mega.some go where
  go :: Parser Char
  go  = choice [ alphaNumChar, char '.', char '-', char '_' ]

lexeme_ :: Text -> Parser ()
lexeme_  = fmap (const ()) . Lex.lexeme sc . Mega.string

char_ :: Char -> Parser ()
char_  = fmap (const ()) . Lex.lexeme sc . Mega.char


------------------------------------------------------------------------------
-- | Space consumer.
sc :: Parser ()
sc  = Lex.space
  space1
  mempty
  (Lex.skipBlockCommentNested "<!--" "-->")
