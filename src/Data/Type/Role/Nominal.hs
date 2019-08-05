{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Role.Nominal
  ( Nominal
  , Nominality(..)
  , applyNominal
  ) where

import Data.Coerce
import Data.Type.Coercion
import Data.Type.Equality ((:~:)(..))

-- | A constraint witnessing that the next argument of the type constructor of
-- the @f@ type has the nominal type role.
--
-- This class is provided only for completeness, everything is automatically an
-- instance of this class.
class (forall a b. a ~ b => Coercible (f a) (f b)) => Nominal (f :: k -> l)
instance Nominal f

-- | A datatype witness of the nominal type role.
data Nominality (f :: k -> l) where
  Nominality :: Nominal f => Nominality f

-- | Saturate a coercion of a nominal datatype.
applyNominal :: Coercion f g -> a :~: b -> Coercion (f a) (g b)
applyNominal Coercion Refl = Coercion
