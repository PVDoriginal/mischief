{-# LANGUAGE AllowAmbiguousTypes #-}

-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module MischiefECS.World where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Functor
import Data.IORef
import Data.Map qualified as Map
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Events.Internal
import MischiefECS.Tables

-- | The World is the main data structure storing the entities, components, archetypes, and so on.
data World = World
  { archetypes :: Archetypes
  , components :: Components
  , entities :: Entities
  , tables :: Tables
  , events :: IORef [ErasedEvent]
  , deferred :: IORef [System ()]
  -- ^ A list of deferred systems that are ran at the end of each system, or can be flushed manually using 'flush'.
  , tick :: IORef Tick
  -- ^ The current Tick, incremented each time a system is ran, used for change detection.
  , lastSystemTick :: Tick
  -- ^ The tick on which the last instance of the current system ran.
  , currentSystemTick :: Tick
  -- ^ The tick on which the current system has started.
  , systemId :: SystemId
  -- ^ ID of the current system.
  , frame :: IORef Frame
  -- ^ The current frame.
  }

newtype Frame = Frame Int deriving (Show, Eq, Ord)

newtype SystemId = SystemId Int deriving (Show, Eq, Ord)

newWorld :: IO World
newWorld = do
  archetypes <- emptyArchetypes
  components <- emptyComponents
  entities <- emptyEntities
  tables <- emptyTables
  deferred <- newIORef []
  events <- newIORef []
  tick <- newIORef (Tick 0)
  frame <- newIORef (Frame 0)

  return
    World
      { archetypes
      , components
      , entities
      , tables
      , events
      , deferred
      , tick
      , lastSystemTick = Tick 0
      , currentSystemTick = Tick 0
      , systemId = SystemId 0
      , frame
      }

setSystemTicks :: Tick -> Tick -> World -> World
setSystemTicks lastSystemTick currentSystemTick World{archetypes, components, entities, tables, deferred, tick, systemId, frame, events} =
  World{archetypes, components, entities, tables, deferred, tick, lastSystemTick, currentSystemTick, systemId, frame, events}

setSystemId :: SystemId -> World -> World
setSystemId systemId World{archetypes, components, entities, tables, deferred, tick, lastSystemTick, currentSystemTick, frame, events} =
  World{archetypes, components, entities, tables, deferred, tick, lastSystemTick, currentSystemTick, systemId, frame, events}

-- | Increment the World's tick.
tick :: System ()
tick = do
  world <- ask
  liftIO $ modifyIORef' world.tick (\(Tick x) -> Tick $ x + 1)

-- | Process a 'BundleElement', turning its 'TypeRep' into a 'ComponentId'.
processBundleElement :: World -> ComponentTicks -> BundleElement -> IO ProcessedBundleElement
processBundleElement world ticks BundleElement{rep, component} =
  do
    id <- getComponentId rep world.components
    return
      ProcessedBundleElement
        { id
        , component =
            ComponentData
              { value = component
              , ticks
              }
        }

-- | Process a set of 'BundleElement's into a 'ProcessedBundleData'.
processBundleElements :: World -> ComponentTicks -> Set BundleElement -> IO ProcessedBundleData
processBundleElements world ticks elements =
  do
    elements <- mapM (processBundleElement world ticks) (Set.toList elements)
    return
      ProcessedBundleData{elements}

-- | Combine two 'ProcessedBundleData's, merging their sets of elements.
combineProcessedBundles :: ProcessedBundleData -> ProcessedBundleData -> ProcessedBundleData
combineProcessedBundles bundle1 bundle2 =
  let elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements)
   in ProcessedBundleData{elements}

-- | Check if a 'ComponentId' is inside a 'ProcessedBundleData'.
isInProcessedBundle :: ProcessedBundleData -> ComponentId -> Bool
isInProcessedBundle ProcessedBundleData{elements} id = id `elem` map (\element -> element.id) elements

-- | Sets the change tick of certain elements of the bundle to the specified 'Tick'.
setChangedTickOfComponents :: ProcessedBundleData -> (ComponentId -> Bool) -> Tick -> ProcessedBundleData
setChangedTickOfComponents ProcessedBundleData{elements} shouldChange tick =
  ProcessedBundleData
    { elements =
        map
          ( \ProcessedBundleElement{id, component = ComponentData{value, ticks = ComponentTicks{added, changed}}} ->
              if shouldChange id
                then ProcessedBundleElement{id, component = ComponentData{value, ticks = ComponentTicks{added, changed = tick}}}
                else ProcessedBundleElement{id, component = ComponentData{value, ticks = ComponentTicks{added, changed}}}
          )
          elements
    }

-- | Sets the added tick of certain elements of the bundle to the specified 'Tick'.
setAddedTickOfComponents :: ProcessedBundleData -> (ComponentId -> Bool) -> Tick -> ProcessedBundleData
setAddedTickOfComponents ProcessedBundleData{elements} shouldChange tick =
  ProcessedBundleData
    { elements =
        map
          ( \ProcessedBundleElement{id, component = ComponentData{value, ticks = ComponentTicks{added, changed}}} ->
              if shouldChange id
                then ProcessedBundleElement{id, component = ComponentData{value, ticks = ComponentTicks{added = tick, changed}}}
                else ProcessedBundleElement{id, component = ComponentData{value, ticks = ComponentTicks{added, changed}}}
          )
          elements
    }

removeComponentFromProcessedBundle :: ComponentId -> ProcessedBundleData -> ProcessedBundleData
removeComponentFromProcessedBundle componentId bundle =
  do
    let elements = filter (\x -> x.id /= componentId) bundle.elements
     in ProcessedBundleData{elements}

-- | Despawn an entity.
despawn :: Entity -> System ()
despawn entity =
  do
    world <- ask
    pointers <- liftIO $ readIORef world.entities.pointers
    case Map.lookup entity pointers of
      Nothing -> undefined
      Just pointer -> do
        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables
        currentPointer <- liftIO $ readIORef pointer

        case Map.lookup currentPointer.archetypeId tables of
          Nothing -> undefined
          Just table -> do
            liftIO $ removeComponentsFromTable currentPointer table

            empty <- liftIO $ tableIsEmpty table

            when empty $ do
              liftIO $ removeTableAndArchetype world currentPointer.archetypeId

            liftIO $ removeEntity entity world.entities

removeTableAndArchetype :: World -> ArchetypeId -> IO ()
removeTableAndArchetype !world !archetype =
  do
    removeArchetypeId archetype world.archetypes
    removeTable archetype world.tables

-- | Remove a component from an entity.
removeComponentFromEntity :: forall c. (Component c) => Entity -> System ()
removeComponentFromEntity entity =
  do
    world <- ask
    componentId <- liftIO $ getComponentId (typeRep $ Proxy @c) world.components
    entityPointers <- liftIO $ readIORef world.entities.pointers

    case Map.lookup entity entityPointers of
      Nothing -> undefined
      Just pointer -> do
        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables
        currentPointer <- liftIO $ readIORef pointer

        case Map.lookup currentPointer.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            when (componentId `elem` currentTable.components) $ do
              collectedComponents <- liftIO $ takeComponentsFromTable currentPointer currentTable

              empty <- liftIO $ tableIsEmpty currentTable
              when empty $ do
                liftIO $ removeTableAndArchetype world currentPointer.archetypeId

              let newBundle = removeComponentFromProcessedBundle componentId collectedComponents
              archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

              liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, pointer)

tryGetEntityComponent :: forall c. (Component c) => World -> Entity -> IO (Maybe c)
tryGetEntityComponent world entity =
  do
    pointers <- readIORef world.entities.pointers
    components <- readIORef world.components.map

    let componentId = Map.lookup (typeRep $ Proxy @c) components

    case componentId of
      Nothing -> return Nothing
      Just componentId -> do
        let pointer = Map.lookup entity pointers

        case pointer of
          Nothing -> return Nothing
          Just pointer ->
            do
              pointer <- readIORef pointer
              tryGetComponentFromTables world.tables pointer componentId

tryGetComponents :: forall c. (Component c) => World -> [ArchetypeId] -> IO [ComponentResult c]
tryGetComponents world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    tryGetComponentsFromTables world.tables archetypes componentId

tryGetTicks :: TypeRep -> World -> [ArchetypeId] -> IO [ComponentTicks]
tryGetTicks rep world archetypes =
  do
    componentId <- getComponentId rep world.components
    tryGetTicksFromTables world.tables archetypes componentId

{- | A System is a set of instructions applied over a World.
It can be added to the app to be ran on a certain schedule.

A system is actually a 'ReaderT' 'World' 'IO', meaning you can 'ask' for the World,
or do IO operations directly by using 'liftIO'.
type System = ReaderT World IO
-}
newtype System a = System (ReaderT World IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader World, MonadFail)

runSystem :: System a -> World -> IO a
runSystem (System r) = runReaderT r

instance PrimMonad System where
  type PrimState System = PrimState IO
  primitive = System . lift . primitive

{- | Defer a command to be ran after the current 'System' is finished,
or when 'flush' is called.
-}
defer :: System a -> System ()
defer !system = do
  world <- ask
  liftIO $ modifyIORef' world.deferred (++ [system $> ()])

-- | Flush the current list of deferred commands.
flush :: System ()
flush = do
  world <- ask
  systems <- liftIO $ readIORef world.deferred

  sequence_ systems

  liftIO $ writeIORef world.deferred []

-- | A 'Component' that's inserted automatically on each 'Entity', but can also be set manually.
newtype Name = Name String
  deriving anyclass (Component)
  deriving newtype (Eq)

instance Show Name where
  show :: Name -> String
  show (Name name) = show name
