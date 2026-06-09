module MischiefECS.Tables where

import Control.Monad (forM, when)
import Data.Foldable (for_)
import Data.IORef
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)
import Data.Vector qualified as Vector
import GHC.Records
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Entities.Internal
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec

newtype Tables = Tables (IORef (Map ArchetypeId Table))

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

tryGetComponentFromTable :: forall c. (Component c) => Table -> EntityPointer -> ComponentId -> IO (Maybe c)
tryGetComponentFromTable table pointer componentId =
  do
    columns <- readIORef table.columns
    let column = Map.lookup componentId columns

    case column of
      Nothing -> return Nothing
      Just column -> tryGetComponentFromColumn column pointer

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

tryGetRelationshipCollectionsFromTable :: forall c. (Component c) => Table -> ComponentId -> IO [RelationshipCollection c]
tryGetRelationshipCollectionsFromTable table (ComponentId (id, _)) =
  do
    columns <- readIORef table.columns
    -- TODO: improve lookup performance for partial tuples

    components' <- forM (Map.toList columns) $ \(ComponentId (id', entityId), column) -> do
      if id == id'
        then do
          components <- tryGetComponentsFromColumn @c column
          return $ Just $ map (,entityId) components
        else return Nothing

    entities <- Vec.toList table.entities
    let components'' = zip (map fst entities) $ transpose $ catMaybes components'
    return $
      map
        (\(entity, components) -> RelationshipCollection {collection = map (\(c, i) -> RelationshipResult {value = c, entity, target = Entity {id = i, gen = 0}}) components, entity})
        components''

tryGetComponentsFromTable :: forall c. (Component c) => Table -> ComponentId -> IO [ComponentResult c]
tryGetComponentsFromTable table componentId =
  do
    columns <- readIORef table.columns
    case Map.lookup componentId columns of
      Nothing -> return []
      Just column -> do
        results <- tryGetComponentsFromColumn @c column
        entities <- Vec.toList table.entities
        return $ zipWith ComponentResult results (map fst entities)

tryGetRelationshipCollectionsFromArchetype :: forall c. (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [RelationshipCollection c]
tryGetRelationshipCollectionsFromArchetype archetype tables componentId =
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetRelationshipCollectionsFromTable table componentId

tryGetComponentsFromArchetype :: forall c. (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [ComponentResult c]
tryGetComponentsFromArchetype archetype tables componentId =
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetComponentsFromTable table componentId

tryGetRelationshipCollectionsFromTables :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [RelationshipCollection c]
tryGetRelationshipCollectionsFromTables (Tables tables) archetypes componentId =
  do
    tables <- readIORef tables
    results <- mapM (\archetype -> tryGetRelationshipCollectionsFromArchetype archetype tables componentId) archetypes
    return $ concat results

tryGetComponentsFromTables :: forall c. (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [ComponentResult c]
tryGetComponentsFromTables (Tables tables) archetypes componentId =
  do
    tables <- readIORef tables
    results <- mapM (\archetype -> tryGetComponentsFromArchetype archetype tables componentId) archetypes
    return $ concat results

data ComponentResult c = ComponentResult {value :: c, entity :: Entity}

instance (Show c) => Show (ComponentResult c) where
  show :: ComponentResult c -> String
  show c = show c.value

instance (Eq c) => Eq (ComponentResult c) where
  (==) :: ComponentResult c -> ComponentResult c -> Bool
  (==) a b = a.value == b.value

instance (Ord c) => Ord (ComponentResult c) where
  compare :: ComponentResult c -> ComponentResult c -> Ordering
  compare a b = compare a.value b.value

data RelationshipResult c = RelationshipResult {value :: c, target :: Entity, entity :: Entity} deriving (Show)

data RelationshipCollection c = RelationshipCollection {collection :: [RelationshipResult c], entity :: Entity} deriving (Show)

instance HasField "targets" (RelationshipCollection c) [Entity] where
  getField :: RelationshipCollection c -> [Entity]
  getField r = map (\x -> x.target) r.collection
