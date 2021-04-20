module Text.HTML.Parser where

import Prelude

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..))
import Data.String (toLower) as String
import Data.String.CodeUnits (fromCharArray) as String
import Text.HTML.Types (Attribute(..), Name(..), Tag(..), Value(..), Tags)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (anyChar, char, eof, regex, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (choice, manyTill, sepBy)


name :: Parser Name
name = Name <$> regex "\\w+"


value :: Parser Value
value = Value <$> (string "=" *> (quoted <|> unquoted))
  where
    quoted = try (char '"' *> regex "[^\"]*" <* char '"')
    unquoted = regex "[^>\n\t ]+"


attribute :: Parser Attribute
attribute = Attribute <$> name <*> value


tagOpen :: Parser Tag
tagOpen = char '<' *> tag <* char '>'
  where
    tag = TagOpen <$> (name <* skipSpaces) <*> attribute `sepBy` regex "[\n\t ]+"


tagClose :: Parser Tag
tagClose = string "</" *> tag <* char '>'
  where
    tag = TagClose <$> name


tagSingle :: Parser Tag
tagSingle = char '<' *> tag <* char '>'
  where
    tag = TagOpen <$> (name <* skipSpaces) <*> attribute `sepBy` regex "[\n\t ]+"


tagText :: Parser Tag
tagText = TagText <$> regex "[^<]+"


tagComment :: Parser Tag
tagComment = string "<!--" *> (TagComment <$> comment)
  where
    toString = String.fromCharArray <<< Array.fromFoldable
    comment = toString <$> manyTill anyChar (string "-->")


tagDoctype :: Parser Tag
tagDoctype = do
  d <- String.toLower <$> string "<!" *> regex "\\w+"
  skipSpaces
  h <- String.toLower <$> regex "\\w+" <* char '>'

  if ( d == "doctype" ) && ( h == "html" )
    then pure TagDoctype
    else fail "Not a valid doctype"


parseTags :: String -> Tags
parseTags html =
  case runParser (manyTill tags eof) html of
    Left _ -> Nil
    Right t -> t
  where
    tags = choice $ try <$>
      [ tagOpen
      , tagClose
      , tagSingle
      , tagComment
      , tagText
      , tagDoctype
      ]
