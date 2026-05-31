-- Adapted from `grow-vector` as the package seems unmaintained
-- Module defines mutable vector that can grow in size automatically when an user
-- adds new elements at the end of vector.
--
-- We reallocate vector with 1.5x length to get amortized append.
module MischiefECS.Vec
  ( Vec (..),
    IOVec,

    -- * Quering info about vector
    length,
    null,
    capacity,

    -- * Creation
    new,
    newSized,

    -- * Quering subvectors
    slice,

    -- * Converting to immutable
    thaw,
    freeze,
    toList,

    -- * Capacity maninuplation
    ensure,
    ensureAppend,

    -- * Accessing individual elements
    read,
    write,
    unsafeRead,
    unsafeWrite,

    -- * Appending to vector
    pushBack,
    unsafePushBack,

    -- * modify an element
    modify,
    modify_,
    modifyM,
    modifyM_,

    -- * general utilities
    swap,
    tap,
    shrink,

    -- * O(1) amortized swap backed operations
    takeSwap,
    removeSwap,

    -- * cloning, very naive approach
    clone,
  )
where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable (for_)
import Data.Primitive.MutVar
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MVector
import Debug.Trace (traceShow)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null, read)

-- | Normal rust-like vector with buffer (the MVector) + len (the len field) + cap (the `buffer` length)
data Vec s a = Vec
  { buffer :: !(MutVar s (MVector s a)),
    len :: !(MutVar s Int)
  }
  deriving (Generic)

type IOVec a = Vec RealWorld a

-- | Return current capacity of the vector (amount of elements that it can fit without realloc)
capacity :: (PrimMonad m) => Vec (PrimState m) a -> m Int
capacity v =
  MVector.length <$> readMutVar v.buffer
{-# INLINE capacity #-}

-- | Return current amount of elements in the vector
length :: (PrimMonad m) => Vec (PrimState m) a -> m Int
length v = readMutVar v.len
{-# INLINE length #-}

-- | Return 'True' if there is no elements inside the vector
null :: (PrimMonad m) => Vec (PrimState m) a -> m Bool
null v = (== 0) <$> length v

-- | Allocation of new growable vector with given capacity.
new :: (PrimMonad m) => Int -> m (Vec (PrimState m) a)
new = newSized 0
{-# INLINE new #-}

-- | Allocation of new growable vector with given filled size and capacity.
-- Elements is not initialized. Capacity must be greater than filled size.
newSized :: (PrimMonad m) => Int -> Int -> m (Vec (PrimState m) a)
newSized n cap = do
  buffer <- MVector.new cap
  buffer <- newMutVar buffer
  len <- newMutVar n
  pure Vec {buffer, len}
{-# INLINEABLE newSized #-}

-- | Yield a part of mutable vector without copying it. The vector must contain at least i+n elements.
slice ::
  (PrimMonad m) =>
  -- | i starting index
  Int ->
  -- | n number of elements
  Int ->
  Vec (PrimState m) a ->
  m (Vec (PrimState m) a)
slice i n v = do
  len <- newMutVar n
  mv <- readMutVar v.buffer
  buffer <- newMutVar $! MVector.slice i n mv
  pure $! Vec {len, buffer}
{-# INLINEABLE slice #-}

-- | Convert immutable vector to grow mutable version. Doesn't allocate additonal memory for appending,
-- use 'ensure' to add capacity to the vector.
thaw ::
  (PrimMonad m) =>
  Vector a ->
  m (Vec (PrimState m) a)
thaw u = do
  buffer <- newMutVar =<< Vector.thaw u
  len <- newMutVar $! Vector.length u
  pure Vec {buffer, len}
{-# INLINEABLE thaw #-}

-- | Freezing growable vector. It will contain only actual elements of the vector not including capacity
-- space, but you should call 'U.force' on resulting vector to not hold the allocated capacity of original
-- vector in memory.
freeze ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  m (Vector a)
freeze v = do
  n <- length v
  mv <- readMutVar v.buffer
  Vector.freeze $ MVector.take n mv
{-# INLINEABLE freeze #-}

toList ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  m [a]
toList vec = Vector.toList <$> freeze vec

ensure_not_oob ::
  (HasCallStack, PrimMonad m) =>
  String ->
  -- | the name of the function
  Int ->
  -- | The element we want to check
  Vec (PrimState m) a ->
  -- | The length of the vec
  m ()
ensure_not_oob fname i vec = do
  len <- length vec
  when (i < 0 || i >= len) $ do
    error $ mconcat [fname, ": index ", show i, " is out bounds ", show len]

-- | Ensure that grow vector has at least given capacity possibly with reallocation.
ensure ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  m ()
ensure v cap = do
  current_cap <- capacity v
  unless (current_cap >= cap) $ do
    buffer <- readMutVar v.buffer
    grown <- MVector.grow buffer (cap - current_cap)
    writeMutVar v.buffer grown
{-# INLINEABLE ensure #-}

-- | Ensure that grow vector has enough space for additonal n elements.
-- We grow vector by 1.5 factor or by required elements count * 1.5.
ensureAppend ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  -- | Additional n elements
  Int ->
  m ()
ensureAppend vec i = do
  len <- length vec
  buf <- readMutVar vec.buffer
  let cap = MVector.length buf
  unless (cap >= len + i) $ do
    -- ugly as shit code would like to fix
    let newCap = ceiling $ max (growFactor * fromIntegral cap) (fromIntegral cap + growFactor * fromIntegral (len + i - cap))
    new_buf <- MVector.grow buf (newCap - cap)
    writeMutVar vec.buffer new_buf
  where
    growFactor :: Double
    growFactor = 1.5
{-# INLINEABLE ensureAppend #-}

-- | Read element from vector at given index.
read ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  -- | Index of element. Must be in [0 .. length) range
  Int ->
  m a
read vec i = do
  ensure_not_oob "Vec.read" i vec
  buf <- readMutVar vec.buffer
  MVector.unsafeRead buf i
{-# INLINEABLE read #-}

-- | Read element from vector at given index, without checking whether the index is inbounds
unsafeRead ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  -- | Index of element. Must be in [0 .. length) range
  Int ->
  m a
unsafeRead vec i = do
  buf <- readMutVar vec.buffer
  MVector.unsafeRead buf i
{-# INLINEABLE unsafeRead #-}

-- | Write down element in the vector at given index.
write ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  -- | Index of element. Must be in [0 .. length) range
  Int ->
  a ->
  m ()
write vec i value = do
  ensure_not_oob "Vec.write" i vec
  buf <- readMutVar vec.buffer
  MVector.unsafeWrite buf i value
{-# INLINEABLE write #-}

-- | Write down element in the vector at given index.
unsafeWrite ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  -- | Index of element. Must be in [0 .. length) range
  Int ->
  a ->
  m ()
unsafeWrite vec i value = do
  buf <- readMutVar vec.buffer
  MVector.unsafeWrite buf i value
{-# INLINEABLE unsafeWrite #-}

modify ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  (a -> a) ->
  m a
modify vec i f = do
  ensure_not_oob "Vec.modify" i vec
  old_val <- read vec i
  write vec i (f old_val)
  pure old_val
{-# INLINEABLE modify #-}

modify_ ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  (a -> a) ->
  m ()
modify_ vec i f = do
  ensure_not_oob "Vec.modify_" i vec
  old_val <- read vec i
  write vec i (f old_val)
{-# INLINEABLE modify_ #-}

modifyM ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  (a -> m a) ->
  m a
modifyM vec i f = do
  ensure_not_oob "Vec.modifyM" i vec
  old_val <- read vec i
  write vec i =<< f old_val
  pure old_val
{-# INLINEABLE modifyM #-}

modifyM_ ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  (a -> m a) ->
  m ()
modifyM_ vec i f = do
  ensure_not_oob "Vec.modifyM_" i vec
  old_val <- read vec i
  write vec i =<< f old_val
{-# INLINEABLE modifyM_ #-}

tap ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  (a -> m ()) ->
  m ()
tap vec i act = do
  ensure_not_oob "Vec.tap" i vec
  act =<< read vec i
{-# INLINE tap #-}

-- | O(1) amortized appending to vector
pushBack ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  a ->
  m ()
pushBack vec value = do
  ensureAppend vec 1
  unsafePushBack vec value
{-# INLINEABLE pushBack #-}

-- | O(1) amortized appending to vector. Doesn't reallocate vector, so
-- there must by capacity - length >= 1.
unsafePushBack ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  a ->
  m ()
unsafePushBack vec a = do
  len <- length vec
  buf <- readMutVar vec.buffer
  MVector.write buf len a
  writeMutVar vec.len (len + 1)
{-# INLINEABLE unsafePushBack #-}

swap ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  -- | index of element #1 to swap
  Int ->
  -- | index of element #2 to swap
  m ()
swap vec i j = do
  when (i /= j) $ do
    ensure_not_oob "Vec.swap" i vec
    ensure_not_oob "Vec.swap" j vec
    buffer <- readMutVar vec.buffer
    MVector.swap buffer i j

shrink ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  m ()
shrink vec amount = do
  old_len <- length vec
  writeMutVar vec.len (old_len - amount)

takeSwap ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  Int ->
  m a
takeSwap vec i = do
  value <- read vec i
  removeSwap vec i
  pure value

removeSwap ::
  (PrimMonad m) =>
  Vec (PrimState m) a ->
  -- | index of element to remove
  Int ->
  m ()
removeSwap vec i = do
  len <- length vec
  swap vec i (len - 1)
  shrink vec 1

clone :: (PrimMonad m) => Vec (PrimState m) a -> m (Vec (PrimState m) a)
clone vec = do
  list <- toList vec
  len <- length vec
  newVec <- new len
  for_ list $ pushBack newVec
  return newVec