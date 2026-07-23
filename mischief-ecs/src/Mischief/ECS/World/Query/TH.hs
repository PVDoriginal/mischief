{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

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
import Mischief.ECS.World.Query.QueryFilter
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
  [qd, qf] -> do
    qd <- qqd qd
    qf <- qqf qf
    return $ AppE (AppE (VarE 'query') qd) qf
  _ -> error "Wrong query format."

qqd :: Text -> Q Exp
qqd str = do
  let types = T.splitOn "," str
  types :: [Exp] <- forM types $ \name ->
    case T.words name of
      (x : xs) | x `elem` ["Val", "val", "V", "v"] -> processVal <$> processWordsQd name xs
      w -> processWordsQd name w

  case types of
    [x] -> return x
    types -> return $ TupE $ map Just types

processWordsQd :: Text -> [Text] -> Q Exp
processWordsQd name words = case words of
  (x : xs) | x `elem` ["Has", "has", "H", "h"] -> processHasQd name xs
  (x : xs) | x `elem` ["Maybe", "maybe", "M", "m"] -> processMaybeQd name xs
  [name] -> processC name
  [name, "*"] -> processR name (ConE 'Any)
  [name, e] -> processR name . VarE =<< getValueName e
  _ -> error $ "Invalid query type: " ++ T.unpack name ++ "."

processMaybeQd :: Text -> [Text] -> Q Exp
processMaybeQd name words = case words of
  [name] -> processM name
  [name, "*"] -> processMR name (ConE 'Any)
  [name, e] -> processMR name . VarE =<< getValueName e
  _ -> error $ "Invalid query type: " ++ T.unpack name ++ "."

processHasQd :: Text -> [Text] -> Q Exp
processHasQd name words = case words of
  [name] -> processH name
  [name, "*"] -> processHR name (ConE 'Any)
  [name, e] -> processHR name . VarE =<< getValueName e
  _ -> error $ "Invalid query type: " ++ T.unpack name ++ "."

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

qqf :: Text -> Q Exp
qqf str = do
  let filters = T.splitOn "," str
  filters <- forM filters $ \filter -> do
    let filters' = T.splitOn "|." filter
    filters' <- forM filters' qFilter

    case filters' of
      [x] -> return x
      filters' -> return $ TupE $ map Just filters'

  case filters of
    [x] -> return x
    filters -> return $ TupE $ map Just filters

qFilter :: Text -> Q Exp
qFilter str = do
  case T.strip str of
    str | p ["With", "with"] str -> processWith $ remFirst str
    str | p ["Without", "without"] str -> processWithout $ remFirst str
    _ -> undefined
  where
    p l x = any (`T.isPrefixOf` x) l

remFirst :: Text -> Text
remFirst = T.dropWhile (/= ' ')

processWith :: Text -> Q Exp
processWith str = AppE (ConE 'With) <$> parseFilterTypes str

processWithout :: Text -> Q Exp
processWithout str = AppE (ConE 'Without) <$> parseFilterTypes str

parseFilterTypes :: Text -> Q Exp
parseFilterTypes str = do
  let types = T.splitOn "." . stripOptionalBrackets $ str
  types <- forM types parseFilterType

  case types of
    [x] -> return x
    types -> return $ TupE $ map Just types

parseFilterType :: Text -> Q Exp
parseFilterType str = do
  case T.words str of
    [name] -> processC name
    [name, "*"] -> processR name (ConE 'Any)
    [name, e] -> processR name . VarE =<< getValueName e
    _ -> error $ "Invalid filter type: " ++ T.unpack str ++ "."

stripOptionalBrackets :: Text -> Text
stripOptionalBrackets t =
  case T.stripPrefix "(" t of
    Nothing -> fromMaybe t (T.stripSuffix ")" t)
    Just t -> fromMaybe t (T.stripSuffix ")" t)
