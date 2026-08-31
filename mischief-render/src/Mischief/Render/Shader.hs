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
import Mischief.Render.Core
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.Functions
import Mischief.Render.Shader.Params
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (abs, cos, sin)

test :: () -> Shader Vec4f
test _ = do
  undefined

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

data Binds f = Binds
  { sampler :: Binding f 0 Sampler,
    texture :: Binding f 1 Texture
  }
  deriving (Generic, Bindable)

data VertexOutput f = VertexOutput
  { position :: BuiltIn f "position" Vec4f,
    uv :: Location f 0 Vec2f
  }
  deriving (Generic, ShaderParam)

newtype FragmentOutput f = FragmentOutput (Location f 0 Vec4f)
  deriving stock (Generic)
  deriving anyclass (ShaderParam)

-- frag :: Binds -> BuiltIn "location" Vec2f -> Shader (Location 0 Vec4f)
-- frag b input = do
--   let result = sample (get b.texture) (get b.sampler) (get input)
--   pure (set result)

shaderToWGSL :: forall a. (ShaderParam a) => Shader (a GPU) -> Text
shaderToWGSL (Shader s) = do
  let (a, ShaderState {ast}) = runState s ShaderState {counter = 0, ast = Empty}
  let ast' = case (paramDefinition @a) of
        StructParam n _ -> do
          let Values vs = collectValues a
          let vst = map valueToWGSL vs
          Concat ast (Return (StructInit n vst))
        AnonParam _ _ -> do
          let Values [v] = collectValues a
          Concat ast (Return (unwrapValue v))
  stmtToWGSL ast'

unwrapValue :: forall a. Value -> Expr a
unwrapValue (Value (a :: Expr b)) = unsafeCoerce a

valueToWGSL :: Value -> Text
valueToWGSL (Value (a :: Expr a)) = exprToWGSL a

genFunction :: forall a. (ShaderParam a) => Text -> Text -> Text -> Shader (a GPU) -> Text
genFunction name input output s = "fn " <> name <> "(" <> input <> ") -> " <> output <> " {\n" <> shaderToWGSL s <> "\n}"

genInput :: ParamDefinition -> Text
genInput (StructParam name _) = "input: " <> name
genInput (AnonParam n m) = n <> "input: " <> m

genOutput :: ParamDefinition -> Text
genOutput (StructParam name _) = name
genOutput (AnonParam n m) = n <> m

genShader :: forall p a b. (Bindable b, ShaderParam p, ShaderParam a) => Text -> (b GPU -> p GPU -> Shader (a GPU)) -> Text
genShader tag s = genBindings (Proxy @b) <> genParams (Proxy @p) <> genParams (Proxy @a) <> "@" <> tag <> "\n" <> genFunction "main" (genInput $ paramDefinition @p) (genOutput $ paramDefinition @a) (s dummyBinding dummyParam)

genBindings :: forall b. (Bindable b) => Proxy b -> Text
genBindings _ =
  let Bindings x = collectBindings (Proxy @(b Internal))
   in T.concat (map genBinding x)

genBinding :: BindingData -> Text
genBinding NormalData {bType, index} = "@group(0) @binding(" <> T.pack (show index) <> ")\nvar b" <> T.pack (show index) <> " : " <> bType <> ";\n\n"
genBinding UniformData {structName, structFields, index} =
  "@group(0) @binding(" <> T.pack (show index) <> ")\nvar<uniform> b" <> T.pack (show index) <> " : " <> structName <> ";\n\n" <> genBuffer structName structFields

genBuffer :: Text -> [(Text, Text)] -> Text
genBuffer name fields =
  let fields' = map (\(x, y) -> "  " <> x <> ": " <> y) fields
   in "struct " <> name <> " {\n" <> T.intercalate ",\n" fields' <> "\n};\n\n"

genParams :: forall p. (ShaderParam p) => Proxy p -> Text
genParams _ =
  let definition = paramDefinition @p
   in case definition of
        StructParam name (Params params) -> "struct " <> name <> " {\n" <> T.concat (map genParam params) <> "};\n\n"
        _ -> ""

genParam :: ParamData -> Text
genParam ParamData {pType, index = BuiltInKind s} = "  @builtin(" <> s <> ") " <> s <> " : " <> pType <> ",\n"
genParam ParamData {pType, index = LocKind n} = "  @location(" <> T.pack (show n) <> ") l" <> T.pack (show n) <> " : " <> pType <> ",\n"

genShaderT :: forall b a p. (Bindable b, ShaderParam p, ShaderParam a) => (b GPU -> p GPU -> Shader (a GPU)) -> String
genShaderT = T.unpack . genShader "no_tag"

t :: () -> () -> Shader Vec2f
t _ _ = do
  let x :: U32 = 5
  let y :: F32 = 5
  z <- var $ cast $ x + cast y
  pure $ vec2 (z, z)
