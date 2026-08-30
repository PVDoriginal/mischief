{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.Render where

import Data.Data
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Quote

data F

data Id

data NatCarry n = NatCarry

type family Field (n :: Nat) a f where
  Field n Int F = Int
  Field n Float F = Float
  Field n Int Id = NatCarry n
  Field n Float Id = NatCarry n

data Test f = Test
  { f1 :: Field 0 Int f,
    f2 :: Field 1 Float f
  }
  deriving (Generic)

instance Default Test

instance Show (Test F) where
  show (Test {f1, f2}) = show f1 ++ ", " ++ show f2

class Default a where
  def :: a F
  default def :: (Generic (a F), GDefault (Rep (a F)) (Rep (a Id))) => a F
  def = to (gdef @(Rep (a F)) @(Rep (a Id)))

class NDefault a b where
  ndef :: a

class GDefault f g where
  gdef :: f p

instance GDefault V1 a where
  gdef = undefined

instance GDefault U1 a where
  gdef = U1

instance (GDefault x1 y1, GDefault x2 y2) => GDefault (x1 :*: x2) (y1 :*: y2) where
  gdef = gdef @x1 @y1 :*: gdef @x2 @y2

instance (NDefault a b) => GDefault (K1 i a) (K1 i' b) where
  gdef = K1 $ ndef @a @b

instance (GDefault f g) => GDefault (M1 i t f) (M1 i t g) where
  gdef = M1 $ gdef @f @g

instance (KnownNat n) => NDefault Int (NatCarry n) where
  ndef = fromInteger $ natVal $ Proxy @n

instance (KnownNat n) => NDefault Float (NatCarry n) where
  ndef = fromInteger $ natVal $ Proxy @n

test :: Test F
test = def
