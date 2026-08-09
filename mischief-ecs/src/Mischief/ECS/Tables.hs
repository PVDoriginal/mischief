{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Tables where

import Control.Monad (forM, when)
import Data.Foldable (Foldable (toList), find, for_)
import Data.IORef
import Data.Kind
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Traversable (for)
import Data.Typeable (Proxy (Proxy), eqT, typeRep, type (:~:) (Refl))
import Data.Vector qualified as Vector
import GHC.Base (Int (..), Word (W#), eqWord#, isTrue#)
import GHC.Records
import GHC.TypeLits
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.EntityDef (Entity# (Entity#), eqEntity#, liftEntity)
import Mischief.ECS.Utils
import Mischief.ECS.Vec (IOVec)
import Mischief.ECS.Vec qualified as Vec

newtype Tables = Tables {inner :: IOVec Table}

data Table = Table
  { columns :: IORef (Map ComponentId Column),
    components :: [ComponentId],
    entities :: IOVec (Entity, IORef EntityPointer)
  }

newtype Column = Column (IOVec ComponentData)

baseline_entities_cap :: Int
baseline_entities_cap = 64

emptyTables :: IO Tables
emptyTables = do
  v <- Vec.new 128
  Vec.pushBack v =<< newTable []
  pure $ Tables v

newTable :: [ComponentId] -> IO Table
newTable components = do
  cols <-
    for components $ \x -> do
      empty_vec <- Vec.new baseline_entities_cap
      pure (x, Column empty_vec)

  columns <- newIORef $ Map.fromList (toList cols)
  entities <- Vec.new baseline_entities_cap

  return $ Table {columns = columns, components, entities}

tableIsEmpty :: Table -> IO Bool
tableIsEmpty table = Vec.null table.entities

-- removeTable :: ArchetypeId -> Tables -> IO ()
-- removeTable archetype tables = do
--   let Tables tables' = tables
--   modifyIORef' tables' $ Map.filterWithKey (\archetype' _ -> archetype' /= archetype)

-- |
--  runs the specified monadic action with the value of the given map key as input, if the key exists.
--  if it doesn't, do nothing
tapMap ::
  (Monad m, Ord k) =>
  Map k a ->
  k ->
  (a -> m ()) ->
  m ()
tapMap map k act = do
  maybe (pure ()) act $ Map.lookup k map

insertComponentsIntoMap :: ProcessedBundleData -> Map ComponentId Column -> IO ()
insertComponentsIntoMap bundle map =
  for_ bundle.elements $ \el ->
    tapMap map el.id $ \(Column col) ->
      Vec.pushBack col el.component

insertComponentsIntoTable :: ProcessedBundleData -> Table -> IO ()
insertComponentsIntoTable bundle table = do
  cols <- readIORef table.columns
  insertComponentsIntoMap bundle cols

replaceComponentsIntoMap ::
  ProcessedBundleData ->
  -- | The current tick that will be as the components' change tick.
  Maybe Tick ->
  EntityPointer ->
  Map ComponentId Column ->
  IO ()
replaceComponentsIntoMap bundle tick (EntityPointer (# archetypeId, rowIndex #)) tableMap = do
  for_ bundle.elements $ \el ->
    tapMap tableMap el.id $ \(Column col) ->
      case tick of
        Just tick ->
          Vec.modify_ col (I# rowIndex) (\ComponentData {value, ticks = ComponentTicks {changed, added}} -> ComponentData {value = el.component.value, ticks = ComponentTicks {changed = tick, added}})
        Nothing ->
          Vec.modify_ col (I# rowIndex) (\ComponentData {value, ticks} -> ComponentData {value = el.component.value, ticks})

--   foldl' modifyMap tableMap bundle.elements
--  where
--   modifyMap tableMap element = Map.adjust (modifyComponents element) element.id tableMap
--   modifyComponents element (Column x) =
--     let (x', _ : ys) = splitAt pointer.rowIndex x
--      in Column $ x' ++ [element.component] ++ ys

replaceComponentsIntoTable ::
  ProcessedBundleData ->
  -- | The current tick that will be as the components' change tick.
  Maybe Tick ->
  EntityPointer ->
  Table ->
  IO ()
replaceComponentsIntoTable bundle tick pointer table = do
  cols <- readIORef table.columns
  replaceComponentsIntoMap bundle tick pointer cols

takeFromColumn :: EntityPointer -> Column -> IO ComponentData
takeFromColumn (EntityPointer (# _, rowIndex #)) (Column col) = Vec.takeSwap col (I# rowIndex)

takeComponentsFromTable :: EntityPointer -> Table -> IO ProcessedBundleData
takeComponentsFromTable (EntityPointer (# archetypeId, rowIndex #)) table = do
  cols <- readIORef table.columns
  newColumns <- for cols $ takeFromColumn (EntityPointer (# archetypeId, rowIndex #))
  removeEntityFromTable (I# rowIndex) table
  let elements = map (uncurry ProcessedBundleElement) $ Map.toList newColumns
  pure ProcessedBundleData {elements}

collectComponentIdsFromTable :: Table -> IO [ComponentId]
collectComponentIdsFromTable table = do
  cols <- readIORef table.columns
  return $ map fst $ Map.toList cols

-- let elements = map getComponent newColumns
-- return ProcessedBundleData{elements}

removeComponentFromColumn :: EntityPointer -> Column -> IO ()
removeComponentFromColumn (EntityPointer (# _, rowIndex #)) (Column col) = Vec.removeSwap col (I# rowIndex)

removeComponentsFromMap :: EntityPointer -> Map ComponentId Column -> IO ()
removeComponentsFromMap pointer columnMap =
  for_ columnMap $ removeComponentFromColumn pointer

-- map (\(id, column) -> (id, removeComponentFromColumn pointer column)) (Map.toList columnMap)

removeComponentsFromTable :: EntityPointer -> Table -> IO ()
removeComponentsFromTable (EntityPointer (# archetypeId, rowIndex #)) table = do
  cols <- readIORef table.columns
  removeComponentsFromMap (EntityPointer (# archetypeId, rowIndex #)) cols
  removeEntityFromTable (I# rowIndex) table

removeRow :: Int -> IOVec (Entity, IORef EntityPointer) -> IO ()
removeRow row_idx vec = do
  len <- Vec.length vec
  Vec.removeSwap vec row_idx
  when (row_idx < len - 1) $ do
    Vec.tap vec row_idx $ \(_, ptr) ->
      modifyIORef' ptr $ \(EntityPointer (# archetypeId, _ #)) ->
        let !(I# id) = row_idx
         in EntityPointer (# archetypeId, id #)

removeEntityFromTable :: Int -> Table -> IO ()
removeEntityFromTable row table = removeRow row table.entities

-- insertResourceIntoTables :: ProcessedBundleData -> Tick -> Tables -> ArchetypeId -> (Entity, IORef EntityPointer) -> IO ()
-- insertResourceIntoTables bundle tick (Tables tables) archetype (entity, pointerRef) =
--   do
--     innerTables <- readIORef tables

--     table <- newTable bundle
--     let newTables = Map.insert archetype table innerTables
--     writeIORef tables newTables

--     rowIndex <- Vec.length table.entities
--     Vec.pushBack table.entities (entity, pointerRef)

--     let !(I# archetype') = archetype.id
--     let !(I# rowIndex') = rowIndex
--     writeIORef pointerRef $ EntityPointer (# archetype', rowIndex' #)

--     insertComponentsIntoTable bundle table

insertEntityIntoTables :: ProcessedBundleData -> Tables -> ArchetypeId -> (Entity, IORef EntityPointer) -> IO ()
insertEntityIntoTables bundle (Tables tables) archetype pointerRef =
  do
    table <- Vec.unsafeRead tables archetype.id

    rowIndex <- Vec.length table.entities
    Vec.pushBack table.entities pointerRef

    let !(I# archetype') = archetype.id
    let !(I# rowIndex') = rowIndex
    writeIORef (snd pointerRef) $ EntityPointer (# archetype', rowIndex' #)

    insertComponentsIntoTable bundle table

tryGetComponentFromColumn :: forall c. (Component c) => Column -> EntityPointer -> IO (Maybe c)
tryGetComponentFromColumn (Column components) (EntityPointer (# _, rowIndex #)) = do
  element <- Vec.unsafeRead components (I# rowIndex)
  pure $ tryGetComponent element.value

tryGetRelCollectionFromTable :: forall c. (Component c) => Table -> Entity -> EntityPointer -> ComponentId -> IO (Maybe [Result (Rel c)])
tryGetRelCollectionFromTable table entity pointer (ComponentId (# id, target #)) =
  do
    columns <- readIORef table.columns
    -- TODO: improve lookup performance for partial tuples

    components' <- forM (Map.toList columns) $ \(ComponentId (# id', target' #), column) -> do
      if isTrue# $ eqWord# id id'
        then do
          case target' of
            Nothing -> return Nothing
            Just entity -> do
              component <- tryGetComponentFromColumn @c column pointer
              return $ fmap (,entity) component
        else return Nothing

    if null $ catMaybes components'
      then
        return Nothing
      else do
        return $ Just $ map (\(value, target) -> Result (Rel value target, entity)) $ catMaybes components'

tryGetComponentFromTable :: forall c. (Component c) => Table -> EntityPointer -> ComponentId -> IO (Maybe c)
tryGetComponentFromTable table pointer componentId =
  do
    columns <- readIORef table.columns
    let column = Map.lookup componentId columns
    case column of
      Nothing -> return Nothing
      Just column -> tryGetComponentFromColumn column pointer

tryGetRelCollectionFromTables :: forall c. (Component c) => Tables -> Entity -> EntityPointer -> ComponentId -> IO (Maybe ([Result (Rel c)]))
tryGetRelCollectionFromTables (Tables tables) entity (EntityPointer (# archetypeId, rowIndex #)) componentId =
  do
    table <- Vec.unsafeRead tables (I# archetypeId)
    tryGetRelCollectionFromTable table entity (EntityPointer (# archetypeId, rowIndex #)) componentId

tryGetComponentFromTables :: forall c. (Component c) => Tables -> EntityPointer -> ComponentId -> IO (Maybe c)
tryGetComponentFromTables (Tables tables) (EntityPointer (# archetypeId, rowIndex #)) componentId =
  do
    table <- Vec.unsafeRead tables (I# archetypeId)
    tryGetComponentFromTable table (EntityPointer (# archetypeId, rowIndex #)) componentId

tryGetTicksFromColumn :: Column -> IO [ComponentTicks]
tryGetTicksFromColumn (Column components) = do
  frozen <- Vec.freeze components
  let x = Vector.map (\x -> x.ticks) frozen
  return $ Vector.toList x

tryGetEntityTicksFromColumn :: Column -> EntityPointer -> IO ComponentTicks
tryGetEntityTicksFromColumn (Column components) (EntityPointer (# _, rowIndex #)) = (\x -> x.ticks) <$> Vec.unsafeRead components (I# rowIndex)

tryGetTicksFromTable :: Table -> ComponentId -> IO [Maybe ComponentTicks]
tryGetTicksFromTable table componentId =
  do
    columns <- readIORef table.columns
    case Map.lookup componentId columns of
      Nothing -> do
        l <- Vec.length table.entities
        return $ map (const Nothing) [1 .. l]
      Just column -> do
        map Just <$> tryGetTicksFromColumn column

tryGetEntityTicksFromTable :: Table -> EntityPointer -> ComponentId -> IO (Maybe ComponentTicks)
tryGetEntityTicksFromTable table pointer componentId =
  do
    columns <- readIORef table.columns
    case Map.lookup componentId columns of
      Nothing -> return Nothing
      Just column -> Just <$> tryGetEntityTicksFromColumn column pointer

tryGetTicksFromArchetype :: ArchetypeId -> IOVec Table -> ComponentId -> IO [Maybe ComponentTicks]
tryGetTicksFromArchetype archetype tables componentId = do
  table <- Vec.unsafeRead tables archetype.id
  tryGetTicksFromTable table componentId

tryGetEntityTicksFromArchetype :: ArchetypeId -> IOVec Table -> EntityPointer -> ComponentId -> IO (Maybe ComponentTicks)
tryGetEntityTicksFromArchetype archetype tables pointer componentId = do
  table <- Vec.unsafeRead tables archetype.id
  tryGetEntityTicksFromTable table pointer componentId

tryGetTicksFromTables :: Tables -> [ArchetypeId] -> ComponentId -> IO [Maybe ComponentTicks]
tryGetTicksFromTables (Tables tables) archetypes componentId =
  do
    results <- mapM (\archetype -> tryGetTicksFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetEntityTicksFromTables :: Tables -> EntityPointer -> ComponentId -> IO (Maybe ComponentTicks)
tryGetEntityTicksFromTables (Tables tables) (EntityPointer (# archetypeId, rowIndex #)) componentId =
  do
    tryGetEntityTicksFromArchetype (ArchetypeId $ I# archetypeId) tables (EntityPointer (# archetypeId, rowIndex #)) componentId

tryGetComponentsFromColumn :: forall c. (Component c) => Column -> IO [c]
tryGetComponentsFromColumn (Column components) = do
  frozen <- Vec.freeze components
  let x = Vector.mapM (\x -> tryGetComponent x.value) frozen
  pure $ maybe [] Vector.toList x

tryGetRelCollectionsFromTable :: forall c. (Component c) => Table -> ComponentId -> IO [(Entity, [Result (Rel c)])]
tryGetRelCollectionsFromTable table (ComponentId (# id, target #)) =
  do
    -- let Just entity = componentId.entity
    columns <- readIORef table.columns
    -- TODO: improve lookup performance for partial tuples

    components' :: [Maybe [(c, Entity)]] <- forM (Map.toList columns) $ \(ComponentId (# id', target' #), column) -> do
      if isTrue# $ eqWord# id id'
        then do
          case target' of
            Nothing -> return Nothing
            Just entity -> do
              components <- tryGetComponentsFromColumn @c column
              return $ Just $ map (,entity) components
        else return Nothing

    entities <- Vec.toList table.entities
    let components'' = zip (map fst entities) $ transpose $ catMaybes components'
    return $
      map
        (\(entity, components) -> (entity, map (\(value, target) -> Result (Rel value target, entity)) components))
        components''

tryGetComponentsFromTable :: forall c. (Component c) => Table -> ComponentId -> IO [(Entity, Result c)]
tryGetComponentsFromTable table componentId =
  do
    columns <- readIORef table.columns
    case Map.lookup componentId columns of
      Nothing -> undefined
      Just column -> do
        results <- tryGetComponentsFromColumn @c column
        entities <- Vec.toList table.entities
        let zipped = zip (map fst entities) results
        return $ map (\(e, r) -> (e, Result (r, e))) zipped

tryGetEntitiesFromTable :: Table -> IO [Entity]
tryGetEntitiesFromTable table =
  do
    entities <- Vec.toList table.entities
    return $ map fst entities

tryGetComponentsFromTableMaybe :: forall c. (Component c) => Table -> ComponentId -> IO [(Entity, Maybe (Result c))]
tryGetComponentsFromTableMaybe table componentId =
  do
    columns <- readIORef table.columns
    case Map.lookup componentId columns of
      Nothing -> do
        entities <- Vec.toList table.entities
        return $ map ((,Nothing) . fst) entities
      Just column -> do
        results <- tryGetComponentsFromColumn @c column
        entities <- Vec.toList table.entities
        return $ zipWith (\x e -> (e, Just $ Result (x, e))) results (map fst entities)

tryGetRelCollectionsFromArchetype :: forall c. (Component c) => ArchetypeId -> IOVec Table -> ComponentId -> IO [(Entity, [Result (Rel c)])]
tryGetRelCollectionsFromArchetype archetype tables componentId = do
  table <- Vec.unsafeRead tables archetype.id
  tryGetRelCollectionsFromTable table componentId

tryGetComponentsFromArchetype :: forall c. (Component c) => ArchetypeId -> IOVec Table -> ComponentId -> IO [(Entity, Result c)]
tryGetComponentsFromArchetype archetype tables componentId = do
  table <- Vec.unsafeRead tables archetype.id
  tryGetComponentsFromTable table componentId

tryGetEntitiesFromArchetype :: ArchetypeId -> IOVec Table -> IO [Entity]
tryGetEntitiesFromArchetype archetype tables = do
  table <- Vec.unsafeRead tables archetype.id
  tryGetEntitiesFromTable table

tryGetComponentsFromArchetypeMaybe :: forall c. (Component c) => ArchetypeId -> IOVec Table -> ComponentId -> IO [(Entity, Maybe (Result c))]
tryGetComponentsFromArchetypeMaybe archetype tables componentId = do
  table <- Vec.unsafeRead tables archetype.id
  tryGetComponentsFromTableMaybe table componentId

tryGetRelCollectionsFromTables :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [(Entity, [Result (Rel c)])]
tryGetRelCollectionsFromTables (Tables tables) archetypes componentId =
  do
    results <- mapM (\archetype -> tryGetRelCollectionsFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetComponentsFromTables :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [(Entity, Result c)]
tryGetComponentsFromTables (Tables tables) archetypes componentId =
  do
    results <- mapM (\archetype -> tryGetComponentsFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetEntitiesFromTables :: Tables -> [ArchetypeId] -> IO [Entity]
tryGetEntitiesFromTables (Tables tables) archetypes =
  do
    results <- mapM (`tryGetEntitiesFromArchetype` tables) archetypes
    return $ concat results

tryGetComponentsFromTablesMaybe :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [(Entity, Maybe (Result c))]
tryGetComponentsFromTablesMaybe (Tables tables) archetypes componentId =
  do
    results <- mapM (\archetype -> tryGetComponentsFromArchetypeMaybe archetype tables componentId) archetypes
    return $ concat results

newtype Result c = Result (c, Entity)

data ErasedResult where
  ErasedResult :: Result c -> ErasedResult

value :: Result c -> c
value (Result (c, _)) = c

type family IsComp a where
  IsComp (Rel a) = False
  IsComp a = True

entityOf :: Result c -> Entity
entityOf (Result (_, e)) = e

instance (Show c) => Show (Result c) where
  show :: Result c -> String
  show = show . value

instance (Eq c) => Eq (Result c) where
  (==) :: Result c -> Result c -> Bool
  (==) a b = value a == value b

instance (Ord c) => Ord (Result c) where
  compare :: Result c -> Result c -> Ordering
  compare a b = compare (value a) (value b)

instance (HasField a b c) => HasField a (Result b) c where
  getField a = getField @a (value a)

class DeepValue' flag c i | flag c -> i where
  deepValue' :: c -> i

instance DeepValue' True (Result c) c where
  deepValue' = value

instance DeepValue' False (Result (Rel c)) c where
  deepValue' x = x.comp

class DeepValue c i | c -> i where
  deepValue :: c -> i

instance (DeepValue' (IsComp c) (Result c) i) => DeepValue (Result c) i where
  deepValue = deepValue' @(IsComp c)
