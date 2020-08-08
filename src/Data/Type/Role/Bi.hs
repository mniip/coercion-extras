{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Data.Type.Role.Bi(
  Phantom2, Representational2, Nominal2, TypeRoles2
) where

import Data.Kind (Constraint)

import Data.Type.Role.Phantom
import Data.Type.Role.Representational
import Data.Type.Role.Nominal

-- | A constraint witnessing that the next two arguments of the type constructor
-- of the @f@ type have the phantom type role.
-- 
-- This class is equivalent to
-- 
-- > TypeRoles2 Phantom Phantom f
-- 
-- but doesn't require @ConstraintKinds@ extension and
-- provides convenient shorthand.
class (Phantom f, forall a. Phantom (f a))
  => Phantom2 (f :: j -> k -> l)
instance (Phantom f, forall a. Phantom (f a))
  => Phantom2 (f :: j -> k -> l)

-- | A constraint witnessing that the next two arguments of the type constructor
-- of the @f@ type have the representational type role.
-- 
-- This class is equivalent to
-- 
-- > TypeRoles2 Representational Representational f
-- 
-- but doesn't require @ConstraintKinds@ extension and
-- provides convenient shorthand.
class (Representational f, forall a. Representational (f a))
  => Representational2 (f :: j -> k -> l)
instance (Representational f, forall a. Representational (f a))
  => Representational2 (f :: j -> k -> l)

-- | A constraint witnessing that the next two arguments of the type constructor
-- of the @f@ type have the nominal type role.
-- 
-- This class is provided only for completeness, everything is automatically an
-- instance of this class.
-- 
-- This class is equivalent to
-- 
-- > TypeRoles2 Nominal Nominal f
class Nominal2 (f :: j -> k -> l)
instance Nominal2 (f :: j -> k -> l)

-- | A constraint witnessing the next two arguments of the type constructor
-- of the type @f@ is @r1@ and @r2@ respectively.
--
-- As 'Nominal' class is provided only for completeness, it may be useful to
-- notice that roles containing @Nominal@ do not need this class.
--
-- > TypeRoles2 Nominal r       f -- Equivalent to (forall a. r (f a))
-- > TypeRoles2 r       Nominal f -- Equivalent to (r f)
class (r1 f, forall a. r2 (f a))
  => TypeRoles2 (r1 :: (j -> k -> l) -> Constraint)
                (r2 :: (k -> l) -> Constraint)
                (f :: j -> k -> l)
instance (r1 f, forall a. r2 (f a)) => TypeRoles2 r1 r2 f
