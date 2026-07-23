module Mischief.ECS.World.Query.TH (q) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
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
import Mischief.ECS.World.Query.Queryable

q :: QuasiQuoter
q =
  QuasiQuoter
    { quoteExp = qquery' . T.pack,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

qquery' :: Text -> Q Exp
qquery' str = case T.splitOn "/" str of
  [qd] -> AppE (VarE 'query) <$> qqd qd
  [qd, qf] -> undefined
  _ -> error "Wrong query format."

qqd :: Text -> Q Exp
qqd str = do
  let types = T.splitOn "," str
  types :: [Exp] <- forM types $ \name -> do
    case T.words name of
      [name] -> do
        t <- getTypeName name
        return $ AppTypeE (ConE 'C) (ConT t)
      [name, "*"] -> do
        t <- getTypeName name
        return $ AppE (AppTypeE (ConE 'R) (ConT t)) (ConE 'Any)
      [name, e] -> do
        t <- getTypeName name
        e <- getValueName e
        return $ AppE (AppTypeE (ConE 'R) (ConT t)) (VarE e)
      _ -> error $ "Invalid query type: " ++ T.unpack name ++ "."

  case types of
    [x] -> return x
    types -> return $ TupE $ map Just types

getTypeName :: Text -> Q Name
getTypeName name = do
  t <- lookupTypeName $ T.unpack name
  return $ fromMaybe (error $ "Invalid type: " ++ T.unpack name ++ ".") t

getValueName :: Text -> Q Name
getValueName name = do
  t <- lookupValueName $ T.unpack name
  return $ fromMaybe (error $ "Invalid value: " ++ T.unpack name ++ ".") t
