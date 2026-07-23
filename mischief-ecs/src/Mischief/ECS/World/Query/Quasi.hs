module Mischief.ECS.World.Query.Quasi where

import Control.Monad
import Data.Maybe
import Data.Text
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
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable

qquery :: QuasiQuoter
qquery =
  QuasiQuoter
    { quoteExp = qquery' . text,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }

qquery' :: Text -> Q Exp
qquery' str = case splitOn "/" str of
  [qd] -> AppE (VarE 'query) <$> qqd qd
  [qd, qf] -> undefined
  _ -> error "Wrong query format"

qqd :: Text -> Q Exp
qqd str = do
  let types = splitOn "," str
  types :: [Name] <- forM types $ \name -> do
    t <- lookupTypeName (unpack name)
    return $ fromMaybe undefined t

  return $ TupE $ Prelude.map (Just . AppTypeE (ConE ''C) . ConT) types