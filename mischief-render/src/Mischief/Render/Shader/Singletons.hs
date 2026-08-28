{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoCUSKs #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE NoStarIsType #-}

module Mischief.Render.Shader.Singletons where

import Data.Singletons.Base.CustomStar
import GHC.TypeLits

data PrimitiveTypes = TInt | TFloat | TUInt deriving (Eq)

data VecLength = VL2 | VL3 | VL4 deriving (Eq)

data Types where
  Primitive :: PrimitiveTypes -> Types
  ArrayType :: Nat -> Types -> Types
  VectorType :: VecLength -> PrimitiveTypes -> Types
  TTexture :: Types
  TSampler :: Types
  deriving (Eq)

genSingletons [''PrimitiveTypes, ''VecLength, ''Types]