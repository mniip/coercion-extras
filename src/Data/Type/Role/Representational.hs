{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Role.Representational
  ( Representational
  , Representation(..)
  , applyRepresentational
  , NonRepresentational
  , NonRepresentation(..)
  , innerNonRepresentational
  ) where

import Data.Coerce
import Data.Type.Coercion
import Data.Type.Equality ((:~:)(..))
import Data.Type.Role.Nominal

-- | A constraint witnessing that the next argument of the type constructor of
-- the @f@ type has representational (or phantom) type role.
--
-- For example @Either@ and @Either a@ both automatically become instances of
-- this class.
class (forall a b. Coercible a b => Coercible (f a) (f b))
  => Representational (f :: k -> l)
instance (forall a b. Coercible a b => Coercible (f a) (f b))
  => Representational f

-- | A datatype witness of the representational type role.
data Representation (f :: k -> l) where
  Representation :: Representational f => Representation f

-- | Apply a coercion of a representable datatype to a coercion of its argument.
applyRepresentational
  :: Representation f -> Coercion f g -> Coercion a b -> Coercion (f a) (g b)
applyRepresentational Representation k@Coercion Coercion
  = Coercion `trans` applyNominal k Refl

-- | Same as 'applyRepresentational' but when we know the representation of @g@
-- and not @f@.
applyRepresentational'
  :: Representation g -> Coercion f g -> Coercion a b -> Coercion (f a) (g b)
applyRepresentational' Representation k@Coercion Coercion
  = applyNominal k Refl `trans` Coercion

-- | A constraint witnessing that the next argument of the type constructor of
-- the @f@ type has a non-representational (nominal) type role.
--
-- For example `Map` automatically becomes an instance of this class.
class (forall a b. Coercible (f a) (f b) => a ~ b)
  => NonRepresentational (f :: k -> l)
instance (forall a b. Coercible (f a) (f b) => a ~ b)
  => NonRepresentational f

-- | A datatype witness of the non-representational type role.
data NonRepresentation (f :: k -> l) where
  NonRepresentation :: NonRepresentational f => NonRepresentation f

-- | Extract equality from coercibility of types in a non-representable
-- position.
innerNonRepresentational
  :: NonRepresentation f -> Coercion (f a) (f b) -> a :~: b
innerNonRepresentational NonRepresentation Coercion = Refl
