{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Mischief.ECS.World.Query.TH (q, qd, qf) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.TH.Common
import Mischief.ECS.World.Query.TH.QD
import Mischief.ECS.World.Query.TH.QF (Qf, pQf, quoteQf)
import Text.Megaparsec (MonadParsec (eof, try), optional, parse, some, (<|>))
import Text.Megaparsec.Char

q :: QuasiQuoter
q =
  QuasiQuoter
    { quoteExp = \str -> do
        let x = parse (whitespace *> (try pGet <|> pQuery) <* eof) "inline_input" (T.pack str)
        case x of
          Left f -> error (show f)
          Right x -> quoteQuery x,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

data QueryBuilder = QueryBuilder Qd (Maybe Qf) (Maybe Text) deriving (Show)

pQuery :: Parser QueryBuilder
pQuery = do
  qd <- pQd
  whitespace

  qf <- optional $ do
    void $ char '/'
    whitespace
    pQf

  pure $ QueryBuilder qd qf Nothing

pGet :: Parser QueryBuilder
pGet = do
  name <- T.pack <$> some alphaNumChar
  whitespace

  void $ char '.'
  whitespace

  QueryBuilder a b _ <- pQuery
  pure $ QueryBuilder a b (Just name)

quoteQuery :: QueryBuilder -> Q Exp
quoteQuery (QueryBuilder qd Nothing Nothing) = AppE (VarE 'mkQuery) <$> quoteQd qd
quoteQuery (QueryBuilder qd Nothing (Just e)) = do
  e <- getTypeName e
  qd <- quoteQd qd
  pure $ AppE (AppE (VarE 'mkGet) (VarE e)) qd
quoteQuery (QueryBuilder qd (Just qf) Nothing) = do
  qd <- quoteQd qd
  qf <- quoteQf qf
  return $ AppE (AppE (VarE 'mkQuery') qd) qf
quoteQuery (QueryBuilder qd (Just qf) (Just e)) = do
  e <- getTypeName e
  qd <- quoteQd qd
  qf <- quoteQf qf
  return $ AppE (AppE (AppE (VarE 'mkGet') (VarE e)) qd) qf

qf :: QuasiQuoter
qf =
  QuasiQuoter
    { quoteExp = \str -> do
        let x = parse (whitespace *> pQf <* eof) "inline_input" (T.pack str)
        case x of
          Left f -> error (show f)
          Right x -> quoteQf x,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

qd :: QuasiQuoter
qd =
  QuasiQuoter
    { quoteExp = \str -> do
        let x = parse (whitespace *> pQd <* eof) "inline_input" (T.pack str)
        case x of
          Left f -> error (show f)
          Right x -> quoteQd x,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }
