module Mischief.ECS.World.Query.Quasi where

import Language.Haskell.TH qualified
import Language.Haskell.TH.Syntax
import Mischief.ECS.Collectable
import Mischief.ECS.Components.Common
import Mischief.ECS.Entities
import Mischief.ECS.Relationships.ChildOf
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable

newtype QErasedStorage = QErasedStorage {inner :: [QErased]} deriving newtype (Semigroup)

data QErased = QErasedC Language.Haskell.TH.Name | QErasedRel Language.Haskell.TH.Name Entity | QErasedRelAny Type

instance EraseIntoStorage Language.Haskell.TH.Name QErasedStorage where
  erase x = QErasedStorage [QErasedC x]

data QR = QR Language.Haskell.TH.Name Entity

instance EraseIntoStorage QR QErasedStorage where
  erase (QR x e) = QErasedStorage [QErasedRel x e]

quoteC :: (Collectable c QErasedStorage) => c -> Q Exp
quoteC e = do
  let c :: QErasedStorage = collect e
  x <-
    mapM
      ( \case
          QErasedC c -> return $ Just $ AppTypeE (ConE 'C) (ConT c)
          QErasedRel c e -> do
            e <- lift e
            return $ Just $ AppE (AppTypeE (ConE 'R) (ConT c)) e
          _ -> undefined
      )
      c.inner

  return $ AppE (VarE 'query) (TupE x)

quoteD :: Q Exp
quoteD = return (AppTypeE (ConE 'C) (ConT ''ChildOf))

quoteE :: Q Exp
quoteE = (pure (LitE (IntegerL 42)))