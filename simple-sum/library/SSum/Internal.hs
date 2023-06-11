{-# LANGUAGE DataKinds, DerivingStrategies, GHC2021, TypeFamilies, UndecidableInstances #-}
module SSum.Internal where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Data.Void (Void)
import GHC.TypeLits (ErrorMessage(Text), TypeError)
import Prelude

type family (+) (a :: Type) (as :: Type) :: Type where
  (+) a (SSum as) = SSum (a ': as)
  (+) a b = SSum '[a, b]
infixr +

type SSum :: [Type] -> Type
newtype SSum as = SSum { unSSum :: SSumRep as }
deriving newtype instance (Eq (SSumRep as)) => Eq (SSum as)
deriving newtype instance (Show (SSumRep as)) => Show (SSum as)

build :: forall a as. (BuildSSum a as) => a -> SSum as
build x = SSum $ build' (Proxy @as) x

type BuildSSum :: Type -> [Type] -> Constraint
class BuildSSum a as where
  build' :: Proxy as -> a -> SSumRep as

instance (TypeError ('Text "BuildSSum: Impossible!")) => BuildSSum a '[] where
  build' = error "Unreachable"

instance BuildSSum a (a ': as) where
  build' _proxy x = Left x

instance {-# OVERLAPPABLE #-} (BuildSSum a as) => BuildSSum a (a' ': as) where
  build' _proxy x = Right $ build' (Proxy @as) x

match :: forall a as. (MatchSSum a as) => SSum as -> Maybe a
match (SSum rep) = match' (Proxy @as) (Proxy @a) rep

type MatchSSum :: Type -> [Type] -> Constraint
class MatchSSum a as where
  match' :: Proxy as -> Proxy a -> SSumRep as -> Maybe a

instance (TypeError ('Text "MatchSSum: Impossible!")) => MatchSSum a '[] where
  match' _proxy1 _proxy2 _rep = error "Unreachable"

instance MatchSSum a (a ': as) where
  match' _proxy1 _proxy2 = either Just (const Nothing)

instance {-# OVERLAPPABLE #-} (MatchSSum a as) => MatchSSum a (a' ': as) where
  match' _proxy1 proxy2 = either (const Nothing) (match' (Proxy @as) proxy2)

ematch :: forall r as. (EMatchSSum r as) => (SSum as, Matchers r as) -> r
ematch (SSum rep, matchers) = ematch' (Proxy @as) (Proxy @r) matchers rep

type EMatchSSum :: Type -> [Type] -> Constraint
class EMatchSSum r as where
  ematch' :: Proxy as -> Proxy r -> Matchers r as -> SSumRep as -> r

instance EMatchSSum r '[] where
  ematch' _proxy1 _proxy2 f rep = f rep

instance (EMatchSSum r as) => EMatchSSum r (a' ': as) where
  ematch' _proxy1 proxy2 (f, rest) = either f (ematch' (Proxy @as) proxy2 rest)

type family Matchers (r :: Type) (as :: [Type]) :: Type where
  Matchers r '[] = Void -> r
  Matchers r (a ': as) = (a -> r, Matchers r as)

type family SSumRep (as :: [Type]) :: Type where
  SSumRep '[] = Void
  SSumRep (a ': as) = Either a (SSumRep as)

(/\) :: a -> b -> (a, b)
(/\) = (,)
infixr /\
