module Mischief.ECS.World.Query.TH.QF where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.TH.Common
import Text.Megaparsec (MonadParsec (notFollowedBy, try), choice, many, optional, satisfy, some, (<|>))
import Text.Megaparsec.Char

data Qf = With' [QfType] | Added' [QfType] | Changed' [QfType] | Not' Qf | Tup' [Qf] | And' Qf Qf | Or' Qf Qf deriving (Show)

data QfType = QfType {name :: Text, compType :: CompType} deriving (Show)

pQf :: Parser Qf
pQf = Tup' . concat <$> pTup pTup'

pTup' :: Parser [Qf]
pTup' = try ((char '(' *> whitespace) *> (concat <$> pTup pTup') <* (char ')' *> whitespace)) <|> (: []) <$> pOr

pOr :: Parser Qf
pOr = do
  s <- pSingle
  whitespace

  or <- optional $ choice [string "||", string "or", string "OR", string "Or"] <* whitespace
  case or of
    Nothing -> return s
    Just _ -> foldr Or' s <$> pTup'

pAnd :: Parser Qf
pAnd = do
  s <- pSingle
  whitespace

  and <- optional $ choice [string "&&", string "and", string "AND", string "And"] <* whitespace
  case and of
    Nothing -> return s
    Just _ -> foldr And' s <$> pTup'

pSingle :: Parser Qf
pSingle = try pWith <|> pWithout <|> pAdded <|> try pChanged <|> pNot

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
  name <- pNameTup <|> T.pack <$> some alphaNumChar
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
quoteQf (With' x) = processTypes (ConE 'With) x
quoteQf (Changed' x) = processTypes (ConE 'Changed) x
quoteQf (Added' x) = processTypes (ConE 'Added) x
quoteQf (Or' x y) = do
  x <- quoteQf x
  y <- quoteQf y
  pure $ AppE (AppE (ConE 'Or) x) y
quoteQf (And' x y) = do
  x <- quoteQf x
  y <- quoteQf y
  pure $ AppE (AppE (ConE 'And) x) y
quoteQf (Not' x) = AppE (ConE 'Not) <$> quoteQf x

processTup :: [Qf] -> Q Exp
processTup [] = pure $ VarE '()
processTup [x] = quoteQf x
processTup (x : xs) = do
  x' <- quoteQf x
  AppE (AppE (ConE 'And) x') <$> processTup xs

processTypes :: Exp -> [QfType] -> Q Exp
processTypes _ [] = pure $ VarE '()
processTypes exp [x] = AppE exp <$> processType x
processTypes exp (x : xs) = do
  x' <- processType x
  AppE (AppE (ConE 'And) (AppE exp x')) <$> processTypes exp xs

processType :: QfType -> Q Exp
processType (QfType {name, compType = Single}) = processC name
processType (QfType {name, compType}) = processR name =<< relExp compType