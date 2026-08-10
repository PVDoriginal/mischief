module Mischief.ECS.World.Query.TH.Common where

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
import Mischief.ECS.World.Query.Markers hiding (Q)
import Mischief.ECS.World.Query.Markers qualified as Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Query.Queryable qualified as Queryable
import Text.Megaparsec (MonadParsec (eof, lookAhead, notFollowedBy, try), Parsec, choice, many, manyTill, noneOf, optional, parseTest, some, (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data CompType = Single | Pair Text | PairAny deriving (Show)

type Parser = Parsec Void Text

pTup :: Parser a -> Parser [a]
pTup p = do
  r <- optional p
  whitespace

  comma <- optional $ string ","
  whitespace

  case r of
    Nothing -> return []
    Just r -> do
      case comma of
        (Just _) -> ([r] ++) <$> pTup p
        _ -> return [r]

whitespace :: Parser ()
whitespace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

pNameTup :: Parser Text
pNameTup = (char '(' *> whitespace) *> pNameRec <* (char ')' *> whitespace)

pNameRec :: Parser Text
pNameRec = do
  s <- T.pack <$> many (alphaNumChar <|> (' ' <$ space1) <|> char ',')
  o <- optional $ (char '(' *> whitespace) *> pNameRec <* (char ')' *> whitespace)

  case o of
    Nothing -> return s
    Just o -> do
      n <- pNameRec
      return $ s <> "(" <> o <> ")" <> n

getTypeName :: Text -> Q Name
getTypeName name = do
  t <- lookupTypeName $ T.unpack name
  return $ fromMaybe (error $ "Invalid type: " ++ T.unpack name ++ ".") t

getValueName :: Text -> Q Name
getValueName name = do
  t <- lookupValueName $ T.unpack name
  return $ fromMaybe (error $ "Invalid value: " ++ T.unpack name ++ ".") t

processC :: Text -> Q Exp
processC name = do
  let t = M.parseType (T.unpack name)
  case t of
    Left e -> error e
    Right t -> return $ AppTypeE (ConE 'C) t

processR :: Text -> Exp -> Q Exp
processR name e = do
  name <- getTypeName name
  return $ AppE (AppTypeE (ConE 'R) (ConT name)) e

relExp :: CompType -> Q Exp
relExp PairAny = return $ ConE 'Any
relExp (Pair x) = VarE <$> getValueName x
relExp _ = undefined