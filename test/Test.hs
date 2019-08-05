{-# LANGUAGE RoleAnnotations #-}
module Main where

import Data.Type.Role.Nominal
import Data.Type.Role.Representational
import Data.Type.Role.Phantom

import Data.Map (Map)
import Data.Proxy
import Data.Set (Set)

testMaybe :: (Nominality Maybe, Representation Maybe, NonPhantomicity Maybe)
testMaybe = (Nominality, Representation, NonPhantomicity)

testEithera :: (Nominality (Either a), Representation (Either a), NonPhantomicity (Either a))
testEithera = (Nominality, Representation, NonPhantomicity)

testEither :: (Nominality Either, Representation Either, NonPhantomicity Either)
testEither = (Nominality, Representation, NonPhantomicity)

testMapk :: (Nominality (Map k), Representation (Map k), NonPhantomicity (Map k))
testMapk = (Nominality, Representation, NonPhantomicity)

testMap :: (Nominality Map, NonRepresentation Map, NonPhantomicity Map)
testMap = (Nominality, NonRepresentation, NonPhantomicity)

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
