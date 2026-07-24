module Mischief.ECS.World.Query.TH.QF where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Haskell.Meta.Parse as M
import Language.Haskell.TH
import Language.Haskell.TH qualified
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Mischief.ECS.Components (Component)
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Query.TH.QD (CompType (..), Parser, pTup, whitespace)
import Mischief.ECS.World.Query.TH.QD qualified as QD
import Text.Megaparsec (MonadParsec (eof, lookAhead, notFollowedBy, try), Parsec, choice, many, manyTill, noneOf, optional, parseTest, some, (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Qf = With' [QfType] | Added' [QfType] | Changed' [QfType] | Not' Qf | Tup' [Qf] | Or' Qf Qf deriving (Show)

data QfType = QfType {name :: Text, compType :: CompType} deriving (Show)

pQf :: Parser Qf
pQf = Tup' <$> pTup'

pTup' :: Parser [Qf]
pTup' = try ((char '(' *> whitespace) *> (concat <$> pTup pTup') <* (char ')' *> whitespace)) <|> (: []) <$> pOr

pOr :: Parser Qf
pOr = do
  s <- pSingle
  whitespace

  or <- optional $ choice [string "|.", string "||", string "or", string "OR", string "Or"] <* whitespace
  case or of
    Nothing -> return s
    Just _ -> foldr Or' s <$> pTup'

pSingle :: Parser Qf
pSingle = try pWith <|> pWithout <|> pAdded <|> pChanged <|> pNot

pNot :: Parser Qf
pNot = do
  void $ choice [string "Not", string "not", string "!"]
  whitespace

  Tup' <$> pTup'

pWith :: Parser Qf
pWith = do
  void $ choice [string "With", string "with"] <* notFollowedBy alphaNumChar
  whitespace
  With' <$> pTypes

pWithout :: Parser Qf
pWithout = do
  void $ choice [string "Without", string "without"] <* notFollowedBy alphaNumChar
  whitespace
  Not' . With' <$> pTypes

pAdded :: Parser Qf
pAdded = do
  void $ choice [string "Added", string "added"] <* notFollowedBy alphaNumChar
  whitespace
  Added' <$> pTypes

pChanged :: Parser Qf
pChanged = do
  void $ choice [string "Changed", string "changed"] <* notFollowedBy alphaNumChar
  whitespace
  Changed' <$> pTypes

pTypes :: Parser [QfType]
pTypes = try ((char '(' *> whitespace) *> (concat <$> pTup pTypes) <* (char ')' *> whitespace)) <|> (: []) <$> pType

pType :: Parser QfType
pType = do
  name <- QD.pNameTup <|> T.pack <$> some alphaNumChar
  whitespace

  target <- optional $ do
    void $ string "->"
    whitespace
    r <- string "*" <|> T.pack <$> some alphaNumChar
    whitespace
    return r

  let compType = case target of
        Nothing -> Single
        Just "*" -> PairAny
        Just e -> Pair e

  return $
    QfType
      { name,
        compType
      }