module MischiefECS.Components where 

import Data.Map
import Data.Kind
import Data.Data (Typeable, TypeRep)
import Data.IORef
import Prelude hiding (lookup)
import Data.Typeable
import Data.List

data ComponentId = ComponentId {
    id :: Integer
} deriving (Show, Eq, Ord) 

data Components = Components { map :: IORef (Map TypeRep ComponentId), counter :: IORef(Integer) }  

emptyComponents :: IO(Components) 
emptyComponents = do 
    map <- newIORef empty
    counter <- newIORef 0 
    return $ Components map counter 

getComponentId :: TypeRep -> Components -> IO(ComponentId)
getComponentId t Components {map, counter} = do 
    map <- readIORef map 
    
    case Data.Map.lookup t map  of 
        Just t -> return t

    result <- readIORef counter 
    modifyIORef counter (+1) 
    return $ ComponentId result 

data ArchetypeId = ArchetypeId {
    id :: Integer
} deriving (Show, Eq, Ord) 

data Archetypes = Archetypes { map :: IORef (Map [ComponentId] ArchetypeId), counter :: IORef(Integer) }

emptyArchetypes :: IO(Archetypes) 
emptyArchetypes = do 
    map <- newIORef empty
    counter <- newIORef 0 
    return $ Archetypes map counter 

getArchetypeId :: [ComponentId] -> Archetypes -> IO(ArchetypeId)
getArchetypeId t Archetypes {map, counter} = do 
    map <- readIORef map 
    
    case Data.Map.lookup t map  of 
        Just t -> return t

    result <- readIORef counter 
    modifyIORef counter (+1) 
    return $ ArchetypeId result 

data ErasedComponent where
  ErasedComponent :: (Typeable c) => c -> ErasedComponent 

data BundleData = BundleData {
    types :: [TypeRep],
    components :: [ErasedComponent]
}

instance Show BundleData where 
  show BundleData{types} = mconcat ["BundleData [", intercalate ", " ts ,"]"]
    where ts = Prelude.map show types


class Bundle b where 
    bundleData :: b -> BundleData  

class Typeable c => Component c where 
  erase :: c -> ErasedComponent
  erase = ErasedComponent 

mergeBundleData :: BundleData -> BundleData -> BundleData
mergeBundleData a b = 
  BundleData {
    types = a.types ++ b.types,
    components = a.components ++ b.components
  }

instance {-# OVERLAPPABLE #-} (Component c) => Bundle c where 
  bundleData c = BundleData { types = [typeOf c], components = [erase c]} 

instance {-# OVERLAPPING #-} (Bundle c1, Bundle c2) => Bundle (c1, c2) where 
  bundleData (c1, c2) = 
    let 
      b1 = bundleData c1 
      b2 = bundleData c2 
    in mergeBundleData b1 b2

instance
 {-# OVERLAPPING #-} (Bundle c1, Bundle c2, Bundle c3) => Bundle (c1, c2, c3) where 
  bundleData (c1, c2, c3) = 
    let 
      b1 = bundleData c1 
      b2 = bundleData c2 
      b3 = bundleData c3 
    in mergeBundleData b1 $ mergeBundleData b1 b3

instance {-# OVERLAPPING #-} (Bundle c1, Bundle c2, Bundle c3, Bundle c4) => Bundle (c1, c2, c3, c4) where 
  bundleData (c1, c2, c3, c4) = 
    let 
      b1 = bundleData c1 
      b2 = bundleData c2 
      b3 = bundleData c3 
      b4 = bundleData c4 
    in mergeBundleData (mergeBundleData b1 b2) (mergeBundleData b3 b4)
