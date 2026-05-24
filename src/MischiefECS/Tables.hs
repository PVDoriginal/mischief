module MischiefECS.Tables where

import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Vec (IOVec)
import MischiefECS.Vec qualified as Vec

newtype Tables = Tables (IORef (Map ArchetypeId Table))

data Table = Table
  { columns :: IORef (Map ComponentId Column)
  , components :: [ComponentId]
  , entities :: IOVec (Entity, IORef EntityPointer)
  }

newtype Column = Column [ErasedComponent]

baseline_entities_cap :: Int
baseline_entities_cap = 256

emptyTables :: IO Tables
emptyTables =
  Tables <$> newIORef Map.empty

newTable :: ProcessedBundleData -> IO Table
newTable components = do
  columns <- newIORef $ Map.fromList $ map (\x -> (x.id, Column [])) components.elements
  entities <- Vec.new baseline_entities_cap

  let componentIds = map (\x -> x.id) components.elements

  return $ Table{columns = columns, components = componentIds, entities}

tableIsEmpty :: Table -> IO Bool
tableIsEmpty table = Vec.null table.entities

insertComponentsIntoMap :: ProcessedBundleData -> Map ComponentId Column -> Map ComponentId Column
insertComponentsIntoMap bundle map = foldl' (\map element -> Map.adjust (\(Column x) -> Column $ x ++ [element.component]) element.id map) map bundle.elements

insertComponentsIntoTable :: ProcessedBundleData -> Table -> IO ()
insertComponentsIntoTable bundle table = do
  modifyIORef' table.columns $ insertComponentsIntoMap bundle

replaceComponentsIntoMap :: ProcessedBundleData -> EntityPointer -> Map ComponentId Column -> Map ComponentId Column
replaceComponentsIntoMap bundle pointer tableMap =
  foldl' modifyMap tableMap bundle.elements
 where
  modifyMap tableMap element = Map.adjust (modifyComponents element) element.id tableMap
  modifyComponents element (Column x) =
    let (x', _ : ys) = splitAt pointer.rowIndex x
     in Column $ x' ++ [element.component] ++ ys

replaceComponentsIntoTable :: ProcessedBundleData -> EntityPointer -> Table -> IO ()
replaceComponentsIntoTable bundle pointer table = do
  modifyIORef' table.columns $ replaceComponentsIntoMap bundle pointer

takeFromColumn :: EntityPointer -> Column -> (ErasedComponent, Column)
takeFromColumn pointer (Column x) =
  let (x', y : ys) = splitAt pointer.rowIndex x
   in (y, Column $ x' ++ ys)

takeComponentsFromTable :: EntityPointer -> Table -> IO ProcessedBundleData
takeComponentsFromTable pointer table =
  do
    columnsInternal <- readIORef table.columns

    let newColumns = map (\(id, column) -> (id, takeFromColumn pointer column)) $ Map.toList columnsInternal
    writeIORef table.columns $ Map.fromList $ map (\(id, (_, column)) -> (id, column)) newColumns
    removeEntityFromTable pointer.rowIndex table

    let elements = map getComponent newColumns
    return ProcessedBundleData{elements}
 where
  getComponent (componentId, (erasedComponent, _)) = ProcessedBundleElement{id = componentId, component = erasedComponent}

removeComponentFromColumn :: EntityPointer -> Column -> Column
removeComponentFromColumn pointer (Column x) =
  let (x', _ : ys) = splitAt pointer.rowIndex x
   in Column $ x' ++ ys

removeComponentsFromMap :: EntityPointer -> Map ComponentId Column -> Map ComponentId Column
removeComponentsFromMap pointer columnMap = Map.fromList $ map (\(id, column) -> (id, removeComponentFromColumn pointer column)) (Map.toList columnMap)

removeComponentsFromTable :: EntityPointer -> Table -> IO ()
removeComponentsFromTable pointer table =
  do
    modifyIORef' table.columns (removeComponentsFromMap pointer)
    removeEntityFromTable pointer.rowIndex table

removeRow :: Int -> IOVec (Entity, IORef EntityPointer) -> IO ()
removeRow row_idx vec = do
  len <- Vec.length vec
  Vec.swap vec row_idx (len - 1)
  Vec.tap vec row_idx $ \(_, ptr) ->
    modifyIORef' ptr $ \EntityPointer{archetypeId, rowIndex} ->
      EntityPointer{archetypeId, rowIndex = row_idx}
  Vec.shrink vec 1
removeEntityFromTable :: Int -> Table -> IO ()
removeEntityFromTable row table = removeRow row table.entities

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

    writeIORef (snd pointerRef) $ EntityPointer{archetypeId = archetype, rowIndex}

    insertComponentsIntoTable bundle table

tryGetComponentFromColumn :: forall c. (Component c) => Column -> EntityPointer -> IO (Maybe c)
tryGetComponentFromColumn (Column components) pointer =
  do
    let element = components !! pointer.rowIndex
    return $ tryGetComponent element

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

tryGetComponentsFromColumn :: forall c. (Component c) => Column -> IO [c]
tryGetComponentsFromColumn (Column components) =
  do
    let x = mapM tryGetComponent components
    case x of
      Nothing -> return []
      Just c -> return c

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

tryGetComponentsFromArchetype :: forall c. (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [ComponentResult c]
tryGetComponentsFromArchetype archetype tables componentId =
  case Map.lookup archetype tables of
    Nothing -> return []
    Just table -> tryGetComponentsFromTable table componentId

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
  (==) :: (Eq c) => ComponentResult c -> ComponentResult c -> Bool
  (==) a b = a.value == b.value

instance (Ord c) => Ord (ComponentResult c) where
  compare :: (Ord c) => ComponentResult c -> ComponentResult c -> Ordering
  compare a b = compare a.value b.value