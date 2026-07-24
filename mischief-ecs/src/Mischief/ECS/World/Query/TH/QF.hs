module Mischief.ECS.World.Query.TH.QF where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Haskell.Meta.Parse
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
import Text.Megaparsec (MonadParsec (eof, lookAhead, notFollowedBy, try), Parsec, choice, many, manyTill, noneOf, optional, parseTest, satisfy, some, (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Qf = With' [QfType] | Added' [QfType] | Changed' [QfType] | Not' Qf | Tup' [Qf] | Or' Qf Qf | Check' CompType Text deriving (Show)

data QfType = QfType {name :: Text, compType :: CompType} deriving (Show)

pQf :: Parser Qf
pQf = Tup' . concat <$> pTup pTup'

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
pSingle = try pWith <|> pWithout <|> pAdded <|> try pChanged <|> pCheck <|> pNot

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

pCheck :: Parser Qf
pCheck = do
  void $ choice [string "Check", string "check"] <* notFollowedBy alphaNumChar
  whitespace

  f <- pF
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

  return $ Check' compType f

pF :: Parser Text
pF = try ((char '(' *> whitespace) *> pfLambda "(" <* whitespace) <|> T.pack <$> some alphaNumChar

pfLambda :: String -> Parser Text
pfLambda str = do
  x <- many (satisfy (/= ')'))
  void $ char ')'
  let str' = str ++ x ++ [')']
  case parseExp str' of
    Left _ -> do
      pfLambda str'
    Right _ -> return $ T.pack str'

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

quoteQf :: Qf -> Q Exp
quoteQf (Tup' qf) = processTup qf
quoteQf (With' x) = AppE (ConE 'With) <$> processTypes x
quoteQf (Changed' x) = AppE (ConE 'Changed) <$> processTypes x
quoteQf (Added' x) = AppE (ConE 'Added) <$> processTypes x
quoteQf (Or' x y) = do
  x <- quoteQf x
  y <- quoteQf y
  return $ AppE (AppE (ConE 'Or) x) y
quoteQf (Not' x) = AppE (ConE 'Not) <$> quoteQf x
quoteQf (Check' c f) = case parseExp (T.unpack f) of
  Left x -> error x
  Right x -> processCheck c x

processCheck :: CompType -> Exp -> Q Exp
processCheck Single f = return $ AppE (ConE 'Check) f
processCheck (Pair e') f = do
  e <- QD.getValueName e'
  return $ AppE (AppE (ConE 'CheckR) (VarE e)) f
processCheck PairAny f = return $ AppE (AppE (ConE 'CheckR) (ConE 'Any)) f

processTup :: [Qf] -> Q Exp
processTup [x] = quoteQf x
processTup t = TupE . map Just <$> forM t quoteQf

processTypes :: [QfType] -> Q Exp
processTypes [x] = processType x
processTypes t = TupE . map Just <$> forM t processType

processType :: QfType -> Q Exp
processType (QfType {name, compType = Single}) = QD.processC name
processType (QfType {name, compType}) = QD.processR name =<< QD.relExp compType