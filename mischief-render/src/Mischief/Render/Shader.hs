{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}

module Mischief.Render.Shader where

import Control.Monad.State hiding (get)
import Data.Data hiding (cast)
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Mischief.ECS.Log (text)
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.Functions
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (abs, cos, sin)

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

data Binds = Binds
  { sampler :: Binding 0 Sampler,
    texture :: Binding 1 Texture2d
  }
  deriving (Generic, Bindable, Default)

data VertexOutput = VertexOutput
  { position :: BuiltIn "position" Vec4f,
    uv :: Location 0 Vec2f
  }
  deriving (Generic, ShaderParam, Default)

newtype FragmentOutput = FragmentOutput (Location 0 Vec4f)
  deriving stock (Generic)
  deriving anyclass (ShaderParam, Default)

frag :: Binds -> BuiltIn "location" Vec2f -> Shader (Location 0 Vec4f)
frag b input = do
  let result = sample (get b.texture) (get b.sampler) (get input)
  pure (set result)

shaderToWGSL :: forall a. (ShaderParam a) => Shader a -> Text
shaderToWGSL (Shader s) = do
  let (a, ShaderState {ast}) = runState s ShaderState {counter = 0, ast = Empty}
  let n = getName (Proxy @a)
  let Values vs = collectValues a
  let vst = map valueToWGSL vs
  let ast' = Concat ast (Return (StructInit n vst))
  stmtToWGSL ast'

valueToWGSL :: Value -> Text
valueToWGSL (Value (a :: Expr a)) = exprToWGSL a

genFunction :: forall a input. (ShaderParam a, ShaderParam input) => Text -> Proxy input -> Shader a -> Text
genFunction name _ s = "fn " <> name <> "(" <> getType (Proxy @input) "input" <> ") -> " <> getType (Proxy @a) "" <> " {\n" <> shaderToWGSL s <> "\n}"

gen :: forall a b p. (Bindable b, ShaderParam p, ShaderParam a) => (b -> p -> Shader a) -> Text
gen s = genBindings (Proxy @b) <> genParams (Proxy @p) <> genFunction "main" (Proxy @p) (s def def)

genBindings :: forall b. (Bindable b) => Proxy b -> Text
genBindings _ =
  let Bindings x = collectBindings (Proxy @b)
   in T.concat (map genBinding x)

genBinding :: BindingData -> Text
genBinding BindingData {bType, index} = "@group(0) @binding(" <> T.pack (show index) <> ")\nvar b" <> T.pack (show index) <> " : " <> bType <> ";\n\n"

genParams :: forall p. (ShaderParam p) => Proxy p -> Text
genParams _ =
  let Params x = collectParams (Proxy @p)
      name = getName (Proxy @p)
   in case name of
        "" -> ""
        name -> "struct " <> name <> " {\n" <> T.concat (map genParam x) <> "};\n\n"

genParam :: ParamData -> Text
genParam ParamData {pType, index = BuiltInParam s} = "  @builtin(" <> s <> ") " <> s <> " : " <> pType <> ",\n"
genParam ParamData {pType, index = LocParam n} = "  @location(" <> T.pack (show n) <> ") l" <> T.pack (show n) <> " : " <> pType <> ",\n"

genShader :: (Bindable b, ShaderParam p, ShaderParam a) => (b -> p -> Shader a) -> String
genShader = T.unpack . gen

t :: () -> () -> Shader Vec2f
t _ _ = do
  let x :: U32 = 5
  let y :: F32 = 5
  z <- var $ cast $ x + cast y
  pure $ vec2 (z, z)
