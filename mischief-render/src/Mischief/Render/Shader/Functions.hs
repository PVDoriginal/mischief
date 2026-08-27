{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.Render.Shader.Functions where

import Data.Data
import GHC.TypeLits
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types

sample :: Texture2d -> Sampler -> Vec2f -> Vec4f
sample tex sampler v =
  Function
    "sampleTexture"
    [ Param tex,
      Param sampler,
      Param v
    ]

abs :: (IsAlgebric a ~ True) => Expr a -> Expr a
abs = Abs

sin :: (IsAlgebric a ~ True) => Expr a -> Expr a
sin x = Function "sin" [Param x]

cos :: (IsAlgebric a ~ True) => Expr a -> Expr a
cos x = Function "cos" [Param x]

dot :: (IsAlgebric a ~ True) => Expr a -> Expr a -> Expr a
dot a b = Function "dot" [Param a, Param b]