module Mischief.ECS.World.Query.TH.QD where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH qualified
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Text.Megaparsec (MonadParsec (eof, lookAhead, try), Parsec, choice, optional, parseTest, some, (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Qd = Val' Qd | Tup [Qd] | Entity' | Type QdType deriving (Show)

data QdType = QdType {name :: Text, compType :: CompType, mod :: Maybe Mod} deriving (Show)

data CompType = Single | Pair Text | PairAny deriving (Show)

data Mod = M' | H' deriving (Show)

type Parser = Parsec Void Text

pQd :: Parser Qd
pQd = do
  Tup <$> pTup

pEl :: Parser Qd
pEl = do
  bracket <- optional $ string "("
  case bracket of
    Nothing -> pSingle
    Just _ -> Tup <$> pTup

pTup :: Parser [Qd]
pTup = do
  r <- optional pEl
  whitespace

  comma <- optional $ string ","
  whitespace

  bracket <- optional $ string ")"
  whitespace

  case r of
    Nothing -> return []
    Just r -> do
      case (comma, bracket) of
        (Just _, Nothing) -> ([r] ++) <$> pTup
        _ -> return [r]

pSingle :: Parser Qd
pSingle = do
  try pEntity <|> try pVal <|> try pMaybe <|> try pHas <|> pType

pEntity :: Parser Qd
pEntity = do
  void $ choice [string "Entity", string "entity", string "E", string "e"]
  whitespace

  return Entity'

pVal :: Parser Qd
pVal = do
  void $ choice [string "Val", string "val", string "V", string "v"]
  whitespace

  Val' <$> pEl

pMaybe :: Parser Qd
pMaybe = do
  void $ choice [string "Maybe", string "maybe", string "M", string "m"]
  whitespace

  Type t <- pType
  case t.mod of
    Just _ -> undefined
    Nothing -> return $ Type t {mod = Just M'}

pHas :: Parser Qd
pHas = do
  void $ choice [string "Has", string "has", string "H", string "h"]
  whitespace

  Type t <- pType
  case t.mod of
    Just _ -> undefined
    Nothing -> return $ Type t {mod = Just H'}

pType :: Parser Qd
pType = do
  name <- T.pack <$> some alphaNumChar
  whitespace

  target <- optional $ string "*" <|> T.pack <$> some alphaNumChar
  whitespace

  let compType = case target of
        Nothing -> Single
        Just "*" -> PairAny
        Just e -> Pair e

  return . Type $
    QdType
      { name,
        compType,
        mod = Nothing
      }

whitespace :: Parser ()
whitespace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

quoteQd :: Qd -> Q Exp
quoteQd (Type QdType {name, compType = Single, mod = Nothing}) = processC name
quoteQd (Type QdType {name, compType = Single, mod = Just M'}) = processM name
quoteQd (Type QdType {name, compType = Single, mod = Just H'}) = processH name
quoteQd (Type QdType {name, compType, mod = Nothing}) = processR name =<< relExp compType
quoteQd (Type QdType {name, compType, mod = Just M'}) = processMR name =<< relExp compType
quoteQd (Type QdType {name, compType, mod = Just H'}) = processHR name =<< relExp compType
quoteQd (Val' qd) = processVal <$> quoteQd qd
quoteQd (Tup []) = return $ ConE '()
quoteQd (Tup [x]) = quoteQd x
quoteQd (Tup t) = TupE <$> mapM (fmap Just . quoteQd) t
quoteQd Entity' = return $ ConE 'E

relExp :: CompType -> Q Exp
relExp PairAny = return $ ConE 'Any
relExp (Pair x) = VarE <$> getValueName x
relExp _ = undefined

processVal :: Exp -> Exp
processVal = AppE (ConE 'Val)

processC :: Text -> Q Exp
processC name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'C) (ConT name)

processM :: Text -> Q Exp
processM name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'M) (ConT name)

processR :: Text -> Exp -> Q Exp
processR name e = do
  name <- getTypeName name
  return $ AppE (AppTypeE (ConE 'R) (ConT name)) e

processMR :: Text -> Exp -> Q Exp
processMR name e = do
  name <- getTypeName name
  return $ AppE (AppTypeE (ConE 'MR) (ConT name)) e

processH :: Text -> Q Exp
processH name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'Has) (ConT name)

processHR :: Text -> Exp -> Q Exp
processHR name e = do
  name <- getTypeName name
  return $ AppE (AppTypeE (ConE 'HasR) (ConT name)) e

getTypeName :: Text -> Q Name
getTypeName name = do
  t <- lookupTypeName $ T.unpack name
  return $ fromMaybe (error $ "Invalid type: " ++ T.unpack name ++ ".") t

getValueName :: Text -> Q Name
getValueName name = do
  t <- lookupValueName $ T.unpack name
  return $ fromMaybe (error $ "Invalid value: " ++ T.unpack name ++ ".") t
