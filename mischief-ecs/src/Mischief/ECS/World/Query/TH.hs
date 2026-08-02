{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Mischief.ECS.World.Query.TH (q, s, g) where

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
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.Common hiding (Name)
import Mischief.ECS.Entities
import Mischief.ECS.Hidden
import Mischief.ECS.Log
import Mischief.ECS.Utils
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable hiding (Q)
import Mischief.ECS.World.Query.TH.QD
import Mischief.ECS.World.Query.TH.QF (Qf, pCheck, pQf, quoteQf)
import Text.Megaparsec (MonadParsec (eof), Parsec, choice, optional, parse, parseTest, runParserT, some, (<|>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

q :: QuasiQuoter
q =
  QuasiQuoter
    { quoteExp = \str -> do
        let x = parse (whitespace *> pQuery <* eof) "inline_input" (T.pack str)
        case x of
          Left f -> error (show f)
          Right x -> quoteQuery x,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

data Query = Query Qd (Maybe Qf) deriving (Show)

pQuery :: Parser Query
pQuery = do
  qd <- pQd
  whitespace

  qf <- optional $ do
    void $ char '/'
    whitespace
    pQf

  pure $ Query qd qf

quoteQuery :: Query -> Q Exp
quoteQuery (Query qd Nothing) = AppE (VarE 'query) <$> quoteQd qd
quoteQuery (Query qd (Just qf)) = do
  qd <- quoteQd qd
  qf <- quoteQf qf
  return $ AppE (AppE (VarE 'query') qd) qf

s :: QuasiQuoter
s =
  QuasiQuoter
    { quoteExp = \str -> do
        let x = parse (whitespace *> pQuery <* eof) "inline_input" (T.pack str)
        case x of
          Left f -> error (show f)
          Right x -> quoteSingle x,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

quoteSingle :: Query -> Q Exp
quoteSingle (Query qd Nothing) = AppE (VarE 'single) <$> quoteQd qd
quoteSingle (Query qd (Just qf)) = do
  qd <- quoteQd qd
  qf <- quoteQf qf
  return $ AppE (AppE (VarE 'single') qd) qf

g :: QuasiQuoter
g =
  QuasiQuoter
    { quoteExp = \str -> do
        let x = parse (whitespace *> pGet <* eof) "inline_input" (T.pack str)
        case x of
          Left f -> error (show f)
          Right x -> quoteGet x,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

pGet :: Parser Query
pGet = do
  qd <- pQd
  pure $ Query qd Nothing

quoteGet :: Query -> Q Exp
quoteGet (Query qd _) = AppE (VarE 'get) <$> quoteQd qd