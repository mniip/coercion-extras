{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Type.Role.Nominal
import Data.Type.Role.Representational
import Data.Type.Role.Phantom

import Data.Type.Role.Bi

import Data.Map (Map)
import Data.Proxy
import Data.Set (Set)

data Witness c f where
  Witness :: c f => Witness c f

testMaybe :: (Nominality Maybe, Representation Maybe, NonPhantomicity Maybe)
testMaybe = (Nominality, Representation, NonPhantomicity)

testEithera :: (Nominality (Either a), Representation (Either a), NonPhantomicity (Either a))
testEithera = (Nominality, Representation, NonPhantomicity)

testEither :: (Nominality Either, Representation Either, NonPhantomicity Either)
testEither = (Nominality, Representation, NonPhantomicity)

testEitherBi :: (Witness Nominal2 Either,
                 Witness Representational2 Either,
                 Witness (TypeRoles2 Representational Representational) Either,
                 Witness (TypeRoles2 NonPhantom NonPhantom) Either)
testEitherBi = (Witness, Witness, Witness, Witness)

testMapk :: (Nominality (Map k), Representation (Map k), NonPhantomicity (Map k))
testMapk = (Nominality, Representation, NonPhantomicity)

testMap :: (Nominality Map, NonRepresentation Map, NonPhantomicity Map)
testMap = (Nominality, NonRepresentation, NonPhantomicity)

testMapBi :: (Witness Nominal2 Map,
              Witness (TypeRoles2 NonRepresentational Representational) Map,
              Witness (TypeRoles2 NonPhantom NonPhantom) Map)
testMapBi = (Witness, Witness, Witness)

testSet :: (Nominality Set, NonRepresentation Set, NonPhantomicity Set)
testSet = (Nominality, NonRepresentation, NonPhantomicity)

testProxy :: (Nominality Proxy, Representation Proxy, Phantomicity Proxy)
testProxy = (Nominality, Representation, Phantomicity)

newtype IdentityT m a = IdentityT { runIdentityT :: m a }

testIdentityTMaybe :: (Nominality (IdentityT Maybe), Representation (IdentityT Maybe), NonPhantomicity (IdentityT Maybe))
testIdentityTMaybe = (Nominality, Representation, NonPhantomicity)

testIdentityTProxy :: (Nominality (IdentityT Proxy), Representation (IdentityT Proxy), Phantomicity (IdentityT Proxy))
testIdentityTProxy = (Nominality, Representation, Phantomicity)

testIdentityTSet :: (Nominality (IdentityT Set), NonRepresentation (IdentityT Set), NonPhantomicity (IdentityT Set))
testIdentityTSet = (Nominality, NonRepresentation, NonPhantomicity)

testIdentityT :: (Nominality IdentityT, Representation IdentityT, NonPhantomicity IdentityT)
testIdentityT = (Nominality, Representation, NonPhantomicity)

data R p r n = R
type role R phantom representational nominal

testR :: (Nominality R, Representation R, Phantomicity R)
testR = (Nominality, Representation, Phantomicity)

testRp :: (Nominality (R p), Representation (R p), NonPhantomicity (R p))
testRp = (Nominality, Representation, NonPhantomicity)

testRpr :: (Nominality (R p r), NonRepresentation (R p r), NonPhantomicity (R p r))
testRpr = (Nominality, NonRepresentation, NonPhantomicity)

main :: IO ()
main = return ()
