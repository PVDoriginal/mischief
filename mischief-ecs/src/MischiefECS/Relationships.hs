module MischiefECS.Relationships where

import MischiefECS.Components
import MischiefECS.Entities

newtype R c = R (c, Entity)

instance (Component c) => Component (R c)
