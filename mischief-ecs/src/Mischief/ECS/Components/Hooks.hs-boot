{-# LANGUAGE RoleAnnotations #-}

module Mischief.ECS.Components.Hooks (ErasedHook, Hooks (Hooks)) where

type role ErasedHook nominal

data ErasedHook c

newtype Hooks c = Hooks [ErasedHook c]

instance Semigroup (Hooks c)
