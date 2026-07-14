module MischiefECS.Tables where

import Control.Monad (forM, when)
import Data.Foldable (for_)
import Data.IORef
import Data.Kind
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Traversable (for)
import Data.Typeable (Proxy (Proxy), eqT, typeRep, type (:~:) (Refl))
import Data.Vector qualified as Vector
import GHC.Records
import GHC.TypeLits
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Utils
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec

newtype Tables = Tables {inner :: IORef (Map ArchetypeId Table)}

data Table = Table
  { columns :: IORef (Map ComponentId Column),
    components :: [ComponentId],
    entities :: IOVec (Entity, IORef EntityPointer)
  }

newtype Column = Column (IOVec ComponentData)

baseline_entities_cap :: Int
baseline_entities_cap = 256

emptyTables :: IO Tables
emptyTables =
  Tables <$> newIORef Map.empty

newTable :: ProcessedBundleData -> IO Table
newTable components = do
  cols <-
    for components.elements $ \x -> do
      empty_vec <- Vec.new 10
      pure (x.id, Column empty_vec)

  columns <- newIORef $ Map.fromList cols
  entities <- Vec.new baseline_entities_cap

  let componentIds = map (\x -> x.id) components.elements

  return $ Table {columns = columns, components = componentIds, entities}

tableIsEmpty :: Table -> IO Bool
tableIsEmpty table = Vec.null table.entities

removeTable :: ArchetypeId -> Tables -> IO ()
removeTable archetype tables = do
  let Tables tables' = tables
  modifyIORef' tables' $ Map.filterWithKey (\archetype' _ -> archetype' /= archetype)

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
replaceComponentsIntoMap bundle tick pointer tableMap = do
  for_ bundle.elements $ \el ->
    tapMap tableMap el.id $ \(Column col) ->
      case tick of
        Just tick ->
          Vec.modify_ col pointer.rowIndex (\ComponentData {value, ticks = ComponentTicks {changed, added}} -> ComponentData {value = el.component.value, ticks = ComponentTicks {changed = tick, added}})
        Nothing ->
          Vec.modify_ col pointer.rowIndex (\ComponentData {value, ticks} -> ComponentData {value = el.component.value, ticks})

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
takeFromColumn pointer (Column col) = Vec.takeSwap col pointer.rowIndex

takeComponentsFromTable :: EntityPointer -> Table -> IO ProcessedBundleData
takeComponentsFromTable pointer table = do
  cols <- readIORef table.columns
  newColumns <- for cols $ takeFromColumn pointer
  removeEntityFromTable pointer.rowIndex table
  let elements = map (uncurry ProcessedBundleElement) $ Map.toList newColumns
  pure ProcessedBundleData {elements}

-- let elements = map getComponent newColumns
-- return ProcessedBundleData{elements}

removeComponentFromColumn :: EntityPointer -> Column -> IO ()
removeComponentFromColumn pointer (Column col) = Vec.removeSwap col pointer.rowIndex

removeComponentsFromMap :: EntityPointer -> Map ComponentId Column -> IO ()
removeComponentsFromMap pointer columnMap =
  for_ columnMap $ removeComponentFromColumn pointer

-- map (\(id, column) -> (id, removeComponentFromColumn pointer column)) (Map.toList columnMap)

removeComponentsFromTable :: EntityPointer -> Table -> IO ()
removeComponentsFromTable pointer table = do
  cols <- readIORef table.columns
  removeComponentsFromMap pointer cols
  removeEntityFromTable pointer.rowIndex table

removeRow :: Int -> IOVec (Entity, IORef EntityPointer) -> IO ()
removeRow row_idx vec = do
  len <- Vec.length vec
  Vec.removeSwap vec row_idx
  when (row_idx < len - 1) $ do
    Vec.tap vec row_idx $ \(_, ptr) ->
      modifyIORef' ptr $ \EntityPointer {archetypeId, rowIndex} ->
        EntityPointer {archetypeId, rowIndex = row_idx}

removeEntityFromTable :: Int -> Table -> IO ()
removeEntityFromTable row table = removeRow row table.entities

insertResourceIntoTables :: ProcessedBundleData -> Tick -> Tables -> ArchetypeId -> (Entity, IORef EntityPointer) -> IO ()
insertResourceIntoTables bundle tick (Tables tables) archetype (entity, pointerRef) =
  do
    innerTables <- readIORef tables

    table <- newTable bundle
    let newTables = Map.insert archetype table innerTables
    writeIORef tables newTables

    rowIndex <- Vec.length table.entities
    Vec.pushBack table.entities (entity, pointerRef)

    writeIORef pointerRef $ EntityPointer {archetypeId = archetype, rowIndex}

    insertComponentsIntoTable bundle table

insertEntityIntoTables :: ProcessedBundleData -> Tables -> ArchetypeId -> (Entity, IORef EntityPointer) -> IO ()
insertEntityIntoTables bundle (Tables tables) archetype pointerRef =
  do
    innerTables <- readIORef tables

    -- gets the correct table (or crates another one and returns it)
    table <- case Map.lookup archetype innerTables of
      Just table -> pure table
      Nothing -> do
        table <- newTable bundle
        let newTables = Map.insert archetype table innerTables
        writeIORef tables newTables
        return table

    rowIndex <- Vec.length table.entities
    Vec.pushBack table.entities pointerRef

    writeIORef (snd pointerRef) $ EntityPointer {archetypeId = archetype, rowIndex}

    insertComponentsIntoTable bundle table

tryGetComponentFromColumn :: forall c. (Component c) => Column -> EntityPointer -> IO (Maybe c)
tryGetComponentFromColumn (Column components) pointer = do
  element <- Vec.read components pointer.rowIndex
  pure $ tryGetComponent element.value

tryGetRelCollectionFromTable :: forall c. (Component c) => Table -> Entity -> EntityPointer -> ComponentId -> IO (Maybe [RelResult c])
tryGetRelCollectionFromTable table entity pointer componentId =
  do
    columns <- readIORef table.columns
    -- TODO: improve lookup performance for partial tuples

    components' <- forM (Map.toList columns) $ \(componentId', column) -> do
      if componentId'.id == componentId.id
        then do
          case componentId'.entity of
            Just entity -> do
              component <- tryGetComponentFromColumn @c column pointer
              return $ fmap (,entity) component
            Nothing -> return Nothing
        else return Nothing

    if null $ catMaybes components'
      then
        return Nothing
      else
        return $ Just $ map (\(value, target) -> RelResult (value, entity, target)) $ catMaybes components'

tryGetComponentFromTable :: forall c. (Component c) => Table -> EntityPointer -> ComponentId -> IO (Maybe c)
tryGetComponentFromTable table pointer componentId =
  do
    columns <- readIORef table.columns
    let column = Map.lookup componentId columns

    case column of
      Nothing -> return Nothing
      Just column -> tryGetComponentFromColumn column pointer

tryGetRelCollectionFromTables :: forall c. (Component c) => Tables -> Entity -> EntityPointer -> ComponentId -> IO (Maybe ([RelResult c]))
tryGetRelCollectionFromTables (Tables tables) entity pointer componentId =
  do
    tables <- readIORef tables
    let table = Map.lookup pointer.archetypeId tables

    case table of
      Nothing -> return Nothing
      Just table -> tryGetRelCollectionFromTable table entity pointer componentId

tryGetComponentFromTables :: forall c. (Component c) => Tables -> EntityPointer -> ComponentId -> IO (Maybe c)
tryGetComponentFromTables (Tables tables) pointer componentId =
  do
    tables <- readIORef tables
    let table = Map.lookup pointer.archetypeId tables

    case table of
      Nothing -> return Nothing
      Just table -> tryGetComponentFromTable table pointer componentId

tryGetTicksFromColumn :: Column -> IO [ComponentTicks]
tryGetTicksFromColumn (Column components) = do
  frozen <- Vec.freeze components
  let x = Vector.map (\x -> x.ticks) frozen
  return $ Vector.toList x

tryGetTicksFromTable :: Table -> ComponentId -> IO [ComponentTicks]
tryGetTicksFromTable table componentId =
  do
    columns <- readIORef table.columns
    case Map.lookup componentId columns of
      Nothing -> return []
      Just column -> tryGetTicksFromColumn column

tryGetTicksFromArchetype :: ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [ComponentTicks]
tryGetTicksFromArchetype archetype tables componentId =
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetTicksFromTable table componentId

tryGetTicksFromTables :: Tables -> [ArchetypeId] -> ComponentId -> IO [ComponentTicks]
tryGetTicksFromTables (Tables tables) archetypes componentId =
  do
    tables <- readIORef tables
    results <- mapM (\archetype -> tryGetTicksFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetComponentsFromColumn :: forall c. (Component c) => Column -> IO [c]
tryGetComponentsFromColumn (Column components) = do
  frozen <- Vec.freeze components
  let x = Vector.mapM (\x -> tryGetComponent x.value) frozen
  pure $ maybe [] Vector.toList x

tryGetRelCollectionsFromTable :: forall c. (Component c) => Table -> ComponentId -> IO [(Entity, [RelResult c])]
tryGetRelCollectionsFromTable table componentId =
  do
    -- let Just entity = componentId.entity
    columns <- readIORef table.columns
    -- TODO: improve lookup performance for partial tuples

    components' :: [Maybe [(c, Entity)]] <- forM (Map.toList columns) $ \(componentId', column) -> do
      if componentId.id == componentId'.id
        then do
          case componentId'.entity of
            Just entity -> do
              components <- tryGetComponentsFromColumn @c column
              return $ Just $ map (,entity) components
            Nothing -> return Nothing
        else return Nothing

    entities <- Vec.toList table.entities
    let components'' = zip (map fst entities) $ transpose $ catMaybes components'
    return $
      map
        (\(entity, components) -> (entity, map (\(value, target) -> RelResult (value, entity, target)) components))
        components''

tryGetComponentsFromTable :: forall c. (Component c) => Table -> ComponentId -> IO [(Entity, Result c)]
tryGetComponentsFromTable table componentId =
  do
    columns <- readIORef table.columns
    case eqT @c @Entity of
      Just Refl -> do
        entities <- Vec.toList table.entities
        return $ map (\(a, _) -> (a, Result (a, a))) entities
      Nothing ->
        case Map.lookup componentId columns of
          Nothing -> undefined
          Just column -> do
            results <- tryGetComponentsFromColumn @c column
            entities <- Vec.toList table.entities
            let zipped = zip (map fst entities) results
            return $ map (\(e, r) -> (e, Result (r, e))) zipped

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

tryGetRelCollectionsFromArchetype :: forall c. (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [(Entity, [RelResult c])]
tryGetRelCollectionsFromArchetype archetype tables componentId =
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetRelCollectionsFromTable table componentId

tryGetComponentsFromArchetype :: forall c. (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [(Entity, Result c)]
tryGetComponentsFromArchetype archetype tables componentId = do
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetComponentsFromTable table componentId

tryGetComponentsFromArchetypeMaybe :: forall c. (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [(Entity, Maybe (Result c))]
tryGetComponentsFromArchetypeMaybe archetype tables componentId =
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetComponentsFromTableMaybe table componentId

tryGetRelCollectionsFromTables :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [(Entity, [RelResult c])]
tryGetRelCollectionsFromTables (Tables tables) archetypes componentId =
  do
    tables <- readIORef tables
    results <- mapM (\archetype -> tryGetRelCollectionsFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetComponentsFromTables :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [(Entity, Result c)]
tryGetComponentsFromTables (Tables tables) archetypes componentId =
  do
    tables <- readIORef tables
    results <- mapM (\archetype -> tryGetComponentsFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetComponentsFromTablesMaybe :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [(Entity, Maybe (Result c))]
tryGetComponentsFromTablesMaybe (Tables tables) archetypes componentId =
  do
    tables <- readIORef tables
    results <- mapM (\archetype -> tryGetComponentsFromArchetypeMaybe archetype tables componentId) archetypes
    return $ concat results

class Value c where
  value :: c a -> a

class EntityOf c where
  entityOf :: c -> Entity

newtype Result c = Result (c, Entity)

instance Value Result where
  value :: Result c -> c
  value (Result (c, _)) = c

instance EntityOf (Result c) where
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

newtype RelResult c = RelResult (c, Entity, Entity)

instance Value RelResult where
  value (RelResult (c, _, _)) = c

instance EntityOf (RelResult c) where
  entityOf :: RelResult c -> Entity
  entityOf (RelResult (_, e, _)) = e

instance (Show c) => Show (RelResult c) where
  show :: RelResult c -> String
  show r = show (value r, target r)

instance (Eq c) => Eq (RelResult c) where
  (==) :: RelResult c -> RelResult c -> Bool
  (==) a b = (value a, target a) == (value b, target b)

instance (Ord c) => Ord (RelResult c) where
  compare :: RelResult c -> RelResult c -> Ordering
  compare a b = compare (value a, target a) (value b, target b)

instance (HasField a b c) => HasField a (RelResult b) c where
  getField a = getField @a (value a)

target :: RelResult c -> Entity
target (RelResult (_, _, t)) = t
