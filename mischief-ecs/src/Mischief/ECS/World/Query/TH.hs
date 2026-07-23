module Mischief.ECS.World.Query.Quasi where

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
  _ -> error "Wrong query format"

qqd :: Text -> Q Exp
qqd str = do
  let types = T.splitOn "," str
  types :: [Name] <- forM types $ \name -> do
    t <- lookupTypeName $ T.unpack name
    return $ fromMaybe (error $ "Invalid type: " ++ T.unpack name) t

  case types of
    [x] -> return $ AppTypeE (ConE 'C) (ConT x)
    types -> return $ TupE $ map (Just . AppTypeE (ConE 'C) . ConT) types