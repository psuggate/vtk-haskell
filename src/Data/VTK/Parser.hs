{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs, LambdaCase,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell,
             TupleSections #-}
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

module Data.VTK.Parser where

import           Control.Lens               (makeLenses, set, (^.))
import           Data.Scientific
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Text.Megaparsec            as Mega
import           Text.Megaparsec.Char       as Mega
import qualified Text.Megaparsec.Char.Lexer as Lex


-- * Defaults
------------------------------------------------------------------------------
type Parser = Parsec Void Text
type ParserErrorBundle = ParseErrorBundle Text Void


-- * VTK parser data-types
------------------------------------------------------------------------------
data VtkFile
  = VtkFile
      { _vtkMeta   :: VtkMeta
      , _vtkPieces :: [VtkPiece]
      }
  deriving (Eq, Generic, Show)

data VtkMeta
  = VtkMeta
      { _vtkType       :: VtkType
      , _vtkVersion    :: Text
      , _vtkCompressor :: Text
      , _vtkByteOrder  :: ()
      }
  deriving (Eq, Generic, Show)

data VtkType
  = VtkUnstructuredGrid
  | VtkTypeUnknown
  deriving (Enum, Eq, Generic, Ord, Show)

data VtkPiece
  = VtkPiece
      { _pieceType   :: VtkType
      , _piecePoints :: VtkPoints
      , _pieceCells  :: VtkCells
      }
  deriving (Eq, Generic, Show)

data VtkPoints
  = VtkPoints
  deriving (Eq, Generic, Show)

data VtkCells
  = VtkCells
  deriving (Eq, Generic, Show)

------------------------------------------------------------------------------
-- | For representing generic XML metadata.
type TagMeta = [(Text, Text)]


-- * Lenses and instances
------------------------------------------------------------------------------
makeLenses ''VtkMeta

-- ** Some standard instances
------------------------------------------------------------------------------
-- | Left-associative combine.
instance Semigroup VtkMeta where
  VtkMeta t v c () <> x = VtkMeta t' v' c' () where
    t' = case t of
      VtkTypeUnknown -> x ^. vtkType
      _              -> t
    v' = case v of
      "" -> x ^. vtkVersion
      _  -> v
    c' = case c of
      "" -> x ^. vtkCompressor
      _  -> c

instance Monoid VtkMeta where
  mempty = VtkMeta VtkTypeUnknown "" "" ()
  {-# INLINE mempty #-}


-- * Top-level parsers
------------------------------------------------------------------------------
parseUnstructuredMesh :: FilePath -> IO (Maybe VtkFile)
parseUnstructuredMesh file = do
  ts <- Text.readFile file
  let re = Mega.runParser (fn <* Mega.eof) file ts
      fn = VtkFile <$> getmeta <*> getbody VtkUnstructuredGrid
  case re of
    Left er -> putStrLn (Mega.errorBundlePretty er) *> pure Nothing
    Right x -> pure $ Just x


-- * More parsers
------------------------------------------------------------------------------
xmlmeta :: Parser ()
xmlmeta  = string "<?xml" *> space1 *> string "version=\"1.0\"" *> space <* string "?>"

getmeta :: Parser VtkMeta
getmeta  = char '<' *> space *> lexeme_ "VTKFile" *> go mempty <* char '>' where
  go :: VtkMeta -> Parser VtkMeta
  go x = Mega.choice
    [ string "type=" *> (set vtkType `flip` x <$> dquoted gettype)
    , string "version=" *> (set vtkVersion `flip` x <$> dquoted version)
    , string "compressor=" *> cmp x
    , string "byte_order=" *> byt x
    ]
  cmp :: VtkMeta -> Parser VtkMeta
  cmp x = set vtkCompressor `flip` x <$> dquoted (string "vtkZLibDataCompressor")
  byt :: VtkMeta -> Parser VtkMeta
  byt x = x <$ dquoted (string "LittleEndian")

gettype :: Parser VtkType
gettype  = Mega.choice [ VtkUnstructuredGrid <$ string "UnstructuredGrid" ]

getbody :: VtkType -> Parser [VtkPiece]
getbody typ = snd <$> tagged tag go where
  tag = Text.pack . drop 3 $ show typ
  go :: Parser [VtkPiece]
  go  = Mega.some $ getpiece typ

getpiece :: VtkType -> Parser VtkPiece
getpiece VtkUnstructuredGrid = snd <$> tagged "Piece" go where
  go :: Parser VtkPiece
  go  = pure $ VtkPiece VtkUnstructuredGrid VtkPoints VtkCells


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
tagged :: Text -> Parser a -> Parser (TagMeta, a)
tagged tag p = (,) <$> xmlopen tag <*> (p <* xmlclose tag)

-- todo: how to handle metadata?
xmlopen :: Text -> Parser TagMeta
xmlopen tag = try (char_ '<' *> lexeme_ tag *> tagmeta <* char_ '>')

tagmeta :: Parser TagMeta
tagmeta  = Mega.many $ Lex.lexeme sc go where
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
