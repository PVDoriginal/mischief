{-# LANGUAGE OverloadedStrings #-}

module Mischief.Render.Shader.Functions where

import Data.Data
import GHC.TypeLits
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types

sample :: forall n m. (KnownNat n, KnownNat m) => Binding n Texture2d -> Binding m Sampler -> Vec2f -> Vec4f
sample _ _ v =
  Function
    "sampleTexture"
    [ Param $ BindingVar $ natVal (Proxy @n),
      Param $ BindingVar $ natVal (Proxy @m),
      Param v
    ]

abs :: Expr a -> Expr a
abs = Abs

sin :: F32 -> F32
sin x = Function "sin" [Param x]

cos :: F32 -> F32
cos x = Function "cos" [Param x]