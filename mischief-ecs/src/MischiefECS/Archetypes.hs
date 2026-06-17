module MischiefECS.Archetypes where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Components

data Archetypes = Archetypes {map :: IORef (Map [ComponentId] ArchetypeId), revMap :: IORef (Map ArchetypeId [ComponentId]), counter :: IORef Int}

-- Construct an empty 'Archetypes'.
emptyArchetypes :: IO Archetypes
emptyArchetypes = do
  map <- newIORef Map.empty
  map' <- newIORef Map.empty
  counter <- newIORef 0
  return $ Archetypes map map' counter

-- | Get the archetype id for a list of component ids. In case this archetype doesn't exist, it will create it.
--
-- This will also update the maps in 'Components' which map component ids to archetypes that contain them.
getOrAddArchetypeId :: [ComponentId] -> Archetypes -> Components -> IO ArchetypeId
getOrAddArchetypeId t Archetypes {map, revMap, counter} Components {archetypes, pairs} = do
  innerMap <- readIORef map

  case Map.lookup t innerMap of
    Just t -> return t
    Nothing -> do
      result <- readIORef counter
      modifyIORef counter (+ 1)

      modifyIORef map $ Map.insert t (ArchetypeId result)
      modifyIORef revMap $ Map.insert (ArchetypeId result) t

      archetypes <- readIORef archetypes
      pairs' <- readIORef pairs
      for_ t $ \t -> do
        case Map.lookup t.id archetypes of
          Nothing -> undefined
          Just list -> modifyIORef' list $ Set.insert $ ArchetypeId result

        when (isJust t.entity) $ do
          case Map.lookup t pairs' of
            Nothing -> do
              s <- newIORef $ Set.singleton $ ArchetypeId result
              modifyIORef' pairs $ Map.insert t s
            Just list -> do
              modifyIORef' list $ Set.insert $ ArchetypeId result

      return $ ArchetypeId result

-- | Get the archetype ID from a list of component IDs.
getArchetypeId :: [ComponentId] -> Archetypes -> IO (Maybe ArchetypeId)
getArchetypeId t Archetypes {map} = do
  innerMap <- readIORef map
  return (Just =<< Map.lookup t innerMap)

removeArchetypeId :: ArchetypeId -> Archetypes -> Components -> IO ()
removeArchetypeId id Archetypes {map, revMap} Components {archetypes, pairs} = do
  revMap' <- readIORef revMap
  case Map.lookup id revMap' of
    Nothing -> undefined
    Just componentList -> do
      modifyIORef' map $ Map.delete componentList

      archetypes <- readIORef archetypes
      pairs <- readIORef pairs
      for_ componentList $ \component -> do
        case Map.lookup component.id archetypes of
          Nothing -> undefined
          Just l -> modifyIORef' l $ Set.delete id

        when (isJust component.entity) $
          case Map.lookup component pairs of
            Nothing -> undefined
            Just l -> modifyIORef' l $ Set.delete id

  modifyIORef' revMap $ Map.delete id

findMatchingArchetypes :: [ComponentId] -> Archetypes -> Components -> IO [([ComponentId], ArchetypeId)]
findMatchingArchetypes components Archetypes {map = map'} Components {archetypes} =
  do
    archetypes' <- readIORef archetypes
    archetypes'' <- forM components $ \component -> do
      maybe undefined readIORef (Map.lookup component.id archetypes')

    map' <- readIORef map'

    case map Set.toList archetypes'' of
      [] -> return []
      h : tail -> do
        let archetypes = foldr intersect h tail
        return $
          mapMaybe
            (\archetype -> find (\(_, id) -> id == archetype) $ Map.toList map')
            archetypes
