module Mischief.ECS.Timer where

import Data.Function
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Time qualified as Time
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.Pipe
import Mischief.ECS.World.Query.Queryable

data Timer = Timer {duration :: Float, elapsed :: Float, mode :: Mode}

data Mode = Once | Repeat

new :: Float -> Mode -> Timer
new duration mode = Timer {duration, mode, elapsed = 0}

tick :: Float -> Timer -> (Timer, Bool)
tick _ timer | timer.elapsed >= timer.duration = (timer, False)
tick x timer | timer.elapsed + x < timer.duration = (timer {elapsed = timer.elapsed + x}, False)
tick _ Timer {duration, elapsed = _, mode = Once} = (Timer {duration, elapsed = duration, mode = Once}, True)
tick x Timer {duration, elapsed, mode = Repeat} = (Timer {duration, elapsed = elapsed + x - duration, mode = Repeat}, True)

qtimer :: forall b a. (Bundle b, Component b) => (b -> Timer) -> (Timer -> b) -> Query System a -> Query System a
qtimer f f' x =
  x
    & qextend (mkQuery (C @b))
    & qjoin (,)
    & qfilterM
      ( \entity (_, a) -> do
          let timer = f a
          delta <- Time.delta
          let (timer', justFinished) = tick delta timer
          insert (f' timer') entity
          pure justFinished
      )
    & qmap fst

qtimer' :: forall b a. (Bundle b, Component b) => (b -> Timer) -> (Timer -> b) -> Float -> Query System a -> Query System a
qtimer' f f' delta x =
  x
    & qextend (mkQuery (C @b))
    & qjoin (,)
    & qfilterM
      ( \entity (_, a) -> do
          let timer = f a
          let (timer', justFinished) = tick delta timer
          insert (f' timer') entity
          pure justFinished
      )
    & qmap fst
