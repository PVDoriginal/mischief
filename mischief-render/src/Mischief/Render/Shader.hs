{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}

module Mischief.Render.Shader where

import Control.Monad.State
import Data.Data hiding (cast)
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Mischief.ECS.Log (text)
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.Functions
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (abs, cos, sin)

-- newtype Value a = Value Expr deriving newtype (Show)

putBindings :: (Bindable b) => b -> Shader ()
putBindings = undefined

--   pure . Value . CodeString $ nameIndex index

-- plus :: Value a -> Value a -> Shader (Value a)
-- plus a b = mkLet (CodeString $ show a ++ " + " ++ show b)

test :: () -> Shader Vec4f
test _ = do
  x <- var $ vec4 (2, 3, 4, 5)
  pure x.xxxx

test2 :: Shader F32
test2 = do
  let x = 2 + 3
  let y = x / 2

  let s = sin y
  let c = cos (y + 3)

  let x = 5
  let y = x / 2.0

  w :: F32 <- var y

  pure $ s + c

data Test = Test
  { sampler :: Binding 0 Sampler,
    texture :: Binding 1 Texture2d
  }
  deriving (Generic, Bindable, Default)

test3 :: Test -> Shader Vec4f
test3 test = do
  pure $ sample test.texture test.sampler (vec2f (0, 0))

genShader :: (Bindable b, ReflType a) => (b -> Shader (Expr a)) -> String
genShader = T.unpack . gen
