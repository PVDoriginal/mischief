module Mischief.ECS.World.Query.TH.QD where

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
import Mischief.ECS.Components (Component, Res (Res))
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers hiding (Q)
import Mischief.ECS.World.Query.Markers qualified as Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Query.Queryable qualified as Queryable
import Mischief.ECS.World.Query.TH.Common
import Mischief.ECS.World.Query.TH.QF qualified as QF
import Text.Megaparsec (MonadParsec (eof, lookAhead, notFollowedBy, try), Parsec, choice, many, manyTill, noneOf, optional, parseTest, some, (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Qd = Val' Qd | Tup [Qd] | Entity' | Type QdType | Trans QdTrans deriving (Show)

data QdType = QdType {name :: Text, compType :: CompType, mod :: Maybe Mod} deriving (Show)

data QdTrans = QdTrans {name :: Text, exp :: Qd, filter :: Maybe QF.Qf, mod :: Maybe Mod} deriving (Show)

data Mod = M' | H' deriving (Show)

pQd :: Parser Qd
pQd = do
  Tup <$> pTup pEl

pEl :: Parser Qd
pEl = try ((char '(' *> whitespace) *> (Tup <$> pTup pEl) <* (char ')' *> whitespace)) <|> pSingle

-- case bracket of
--   Nothing -> pSingle
--   Just _ -> Tup <$> pTup pEl

data TestG a b = TestG deriving (Component)

pSingle :: Parser Qd
pSingle = do
  try pEntity <|> try pMaybe <|> try pHas <|> try pValStar <|> try (pTrans Nothing) <|> pType Nothing

pEntity :: Parser Qd
pEntity = do
  void $ choice [string "Entity", string "entity", string "E", string "e"] <* notFollowedBy alphaNumChar
  whitespace

  return Entity'

pValStar :: Parser Qd
pValStar = do
  void $ char '*'
  whitespace
  Val' <$> pEl

pMaybe :: Parser Qd
pMaybe = do
  void $ choice [string "Maybe", string "maybe", string "M", string "m"] <* notFollowedBy alphaNumChar
  whitespace

  try (pTrans (Just M')) <|> pType (Just M')

pHas :: Parser Qd
pHas = do
  void $ choice [string "Has", string "has", string "H", string "h"] <* notFollowedBy alphaNumChar
  whitespace

  try (pTrans (Just H')) <|> pType (Just H')

pTrans :: Maybe Mod -> Parser Qd
pTrans mod = do
  name <- pNameTup <|> T.pack <$> some alphaNumChar
  whitespace

  void $ string "->"
  whitespace

  qd <- (char '(' *> whitespace) *> (Tup <$> pTup pEl) <* whitespace

  qf <- optional $ do
    void $ char '/'
    whitespace
    QF.pQf

  whitespace
  void $ char ')'
  whitespace

  return . Trans $
    QdTrans
      { name,
        mod,
        filter = qf,
        exp = qd
      }

pRes :: Maybe Mod -> Parser Qd
pRes mod = do
  void $ choice ["Res", "res"]
  whitespace

  name <- pNameTup <|> T.pack <$> some alphaNumChar
  whitespace

  pure . Type $
    QdType
      { name,
        compType = Resource,
        mod
      }

pType :: Maybe Mod -> Parser Qd
pType mod =
  pRes mod <|> pTypeSimple
  where
    pTypeSimple = do
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

      return . Type $
        QdType
          { name,
            compType,
            mod
          }

pName :: Parser Text
pName = T.pack <$> some alphaNumChar <|> pNameTup

pTypeGeneric :: Parser Text
pTypeGeneric = T.pack <$> (char '{' *> manyTill L.charLiteral (char '}'))

quoteQd :: Qd -> Q Exp
quoteQd (Type QdType {name, compType = Resource, mod = Nothing}) = processRes name
quoteQd (Type QdType {name, compType = Resource, mod = Just M'}) = processMRes name
quoteQd (Type QdType {name, compType = Resource, mod = Just H'}) = processHasRes name
quoteQd (Type QdType {name, compType = Single, mod = Nothing}) = processC name
quoteQd (Type QdType {name, compType = Single, mod = Just M'}) = processM name
quoteQd (Type QdType {name, compType = Single, mod = Just H'}) = processH name
quoteQd (Type QdType {name, compType, mod = Nothing}) = processR name =<< relExp compType
quoteQd (Type QdType {name, compType, mod = Just M'}) = processMR name =<< relExp compType
quoteQd (Type QdType {name, compType, mod = Just H'}) = processHR name =<< relExp compType
quoteQd (Trans QdTrans {name, exp, mod = Nothing, filter}) = processR name =<< relTrans exp filter
quoteQd (Trans QdTrans {name, exp, mod = Just M', filter}) = processMR name =<< relTrans exp filter
quoteQd (Trans QdTrans {name, exp, mod = Just H', filter}) = processHR name =<< relTrans exp filter
quoteQd (Val' qd) = processVal <$> quoteQd qd
quoteQd (Tup []) = return $ ConE '()
quoteQd (Tup [x]) = quoteQd x
quoteQd (Tup t) = TupE <$> mapM (fmap Just . quoteQd) t
quoteQd Entity' = return $ ConE 'E

relTrans :: Qd -> Maybe QF.Qf -> Q Exp
relTrans exp Nothing = AppE (ConE 'Markers.Q) <$> quoteQd exp
relTrans exp (Just f) = do
  qd <- quoteQd exp
  qf <- QF.quoteQf f

  pure $ AppE (AppE (ConE 'Markers.Q') qd) qf

processVal :: Exp -> Exp
processVal = AppE (ConE 'Val)

processRes :: Text -> Q Exp
processRes name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'Res) (ConT name)

processMRes :: Text -> Q Exp
processMRes name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'MRes) (ConT name)

processHasRes :: Text -> Q Exp
processHasRes name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'HasRes) (ConT name)

processM :: Text -> Q Exp
processM name = do
  name <- getTypeName name
  return $ AppTypeE (ConE 'M) (ConT name)

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
