{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Role.Phantom
  ( Phantom
  , Phantomicity(..)
  , applyPhantom
  , NonPhantom
  , NonPhantomicity(..)
  , innerNonPhantom
  ) where

import Data.Coerce
import Data.Type.Coercion
import Data.Type.Role.Nominal
import Data.Type.Equality ((:~:)(..))

-- | A constraint witnessing that the next argument of the type constructor of
-- the @f@ type has phantom type role.
--
-- For example @Proxy@ atomatically becomes an instance of this class.
class (forall a b. Coercible (f a) (f b)) => Phantom (f :: k -> l)
instance (forall a b. Coercible (f a) (f b)) => Phantom f

-- | A datatype witness of the phantom type role.
data Phantomicity (f :: k -> l) where
  Phantomicity :: Phantom f => Phantomicity f

-- | Saturate a coercion of a phantom datatype.
applyPhantom :: Phantomicity f -> Coercion f g -> Coercion (f a) (g b)
applyPhantom Phantomicity k@Coercion = Coercion `trans` applyNominal k Refl

-- | Same as 'applyPhantom' but when we know the phantomicity of @g@
-- and not @f@.
applyPhantom' :: Phantomicity g -> Coercion f g -> Coercion (f a) (g b)
applyPhantom' Phantomicity k@Coercion = applyNominal k Refl `trans` Coercion

-- | A constraint witnessing that the next argument of the type constructor of
-- the @f@ type has a non-phantom (representational or nominal) type role.
--
-- For example @Map@ and @Maybe@ automatically become instances of this class.
class (forall a b. Coercible (f a) (f b) => Coercible a b)
  => NonPhantom (f :: k -> l)
instance (forall a b. Coercible (f a) (f b) => Coercible a b)
  => NonPhantom f

-- | A datatype witness of the non-phantom type role.
data NonPhantomicity (f :: k -> l) where
  NonPhantomicity :: NonPhantom f => NonPhantomicity f

-- | Extract coercion from coercibility of types in a non-phantom position.
innerNonPhantom :: NonPhantomicity f -> Coercion (f a) (f b) -> Coercion a b
innerNonPhantom NonPhantomicity Coercion = Coercion
