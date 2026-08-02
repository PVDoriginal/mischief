module Mischief.ECS.World.Query.QueryType where

import Mischief.ECS.Components
import Mischief.ECS.Mappable

type QueryType a = (Mappable MapQueryVal a a, Mappable MapQueryValidity a a, Component a)
