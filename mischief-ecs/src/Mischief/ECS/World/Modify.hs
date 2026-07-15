module Mischief.ECS.World.Modify where

import Data.Maybe
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Log
import Mischief.ECS.Prelude
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable

-- | Modifies the value of the component with the gievn function.
--
-- The function will be applied over the @live value@ of the component, adding some overhead.
--
-- If you are confident the value in the 'Result' is the live one, or otherwise do not care of updating the live value,
-- you are encouraged to use 'modify'' instead.
--
-- Note that this will trigger change detection even if the provided function is 'id'.
modify :: forall c. (Queryable c (Result c), Bundle c) => Result c -> (c -> c) -> System ()
modify !result !f = do
  res <- get @c (entityOf result)
  case res of
    Nothing -> warn $ "Modify failed: Entity " <> text (entityOf result) <> " is not alive."
    Just res -> insert (f $ value res) (entityOf res)

-- | Modifies the value of the component with the given function.
--
-- The function will be applied over the value contained within the 'Result', which is not guaranteed
-- to be the live value of the component.
--
-- If you wish to apply the function over the live value, use 'modify' instead.
--
-- Note that this will trigger change detection even if the provided function is 'id'.
modify' :: forall c. (Bundle c) => Result c -> (c -> c) -> System ()
modify' !result !f = insert (f . value $ result) (entityOf result)

-- | The most generic function for modifying a component on a given entity.
--
-- This can do removals, insertions, and modify the value.
--
-- Example:
--
-- @
-- data Counter = Counter 'Int' deriving ('Component', 'Queryable')
--
-- incrementCounter :: 'Entity' -> 'System' ()
-- incrementCounter = 'alter' (\case 'Nothing' -> 'Just' $ Counter 0; 'Just' (Counter x) -> 'Just' $ Counter (x + 1))
-- @
alter :: forall c. (Queryable c (Result c), Bundle c, Component c) => (Maybe c -> Maybe c) -> Entity -> System ()
alter !f !entity = do
  val <- get @c entity
  let r = f (fmap value val)

  if isNothing r && isJust val
    then
      remove @c entity
    else case r of
      Just r ->
        insert r entity
      Nothing -> return ()
