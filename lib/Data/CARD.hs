{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Data.CARD
  ( CARD (..)
  , Ef (..)
  , Cr (..)
  -- * Effects
  , Effect
  , ef
  , ef0
  , (|<<|)
  , (|>>|)
  , runEffect
  -- * Consistency refinements
  , Conref
  , cr
  , crT
  , crEqv
  , (|&|)
  , impl
  , checkLe
  , checkBlock
  , checkBlock'
  -- * Common instances
  , Counter (..)
  , RGArray (..)
  ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Generics
import Data.Aeson


class (Eq (Ef s), Eq (Cr s), Ord (Ef s), Ord (Cr s)) => CARD s where
  data Ef s
  defineEffect :: s -> Ef s -> s

  data Cr s
  defineLe :: Cr s -> Effect s -> Effect s -> Bool
  defineLe c e1 (Effect es) | e1 == ef0 = 
    and $ map (not . defineConflict c) es
  defineLe _ _ _ = False

  defineConflict :: Cr s -> Ef s -> Bool
  defineConflict c e = defineLe c ef0 (ef e)


newtype Effect s = Effect [Ef s] deriving (Generic)

instance (Ord (Ef s)) => Semigroup (Effect s) where
  (<>) = (|<<|)

instance (Ord (Ef s)) => Monoid (Effect s) where
  mempty = ef0

deriving instance (CARD s, Read (Ef s)) => Read (Effect s)
deriving instance (CARD s, Show (Ef s)) => Show (Effect s)
deriving instance (CARD s) => Eq (Effect s)
deriving instance (CARD s) => Ord (Effect s)

instance (ToJSON (Ef s)) => ToJSON (Effect s) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Ef s), FromJSON (Ef s)) => FromJSON (Effect s)

ef :: Ef s -> Effect s
ef e = Effect [e]

-- | The no-op effect
ef0 :: Effect s
ef0 = Effect []

-- | Compose effects right-to-left.
(|<<|) :: Effect s -> Effect s -> Effect s
(|<<|) (Effect e2) (Effect e1) = Effect (e1 ++ e2)

-- | Compose effect left-to-right.
(|>>|) :: Effect s -> Effect s -> Effect s
(|>>|) = flip (|<<|)

runEffect :: (CARD s) => s -> Effect s -> s
runEffect s (Effect es) = foldl' defineEffect s es

data Conref s = Conref (Set (Cr s)) | EQV deriving (Generic)

instance (Ord (Cr s)) => Semigroup (Conref s) where
  (<>) (Conref s1) (Conref s2) = Conref (s1 <> s2)
  (<>) EQV _ = EQV
  (<>) _ EQV = EQV

instance (Ord (Cr s)) => Monoid (Conref s) where
  mempty = crT

deriving instance (CARD s) => Eq (Conref s)
deriving instance (CARD s) => Ord (Conref s)
deriving instance (Show (Cr s)) => Show (Conref s)

instance (ToJSON (Cr s)) => ToJSON (Conref s) where
  toEncoding = genericToEncoding defaultOptions

instance (Ord (Cr s), FromJSON (Cr s)) => FromJSON (Conref s)

cr :: Cr s -> Conref s
cr c = Conref (Set.singleton c)

-- | The trivial conref.
crT :: Conref s
crT = Conref (Set.empty)

-- | The complete conref.
crEqv :: Conref s
crEqv = EQV

-- | Conjunction for conrefs.
(|&|) :: (CARD s) => Conref s -> Conref s -> Conref s
(|&|) (Conref c1) (Conref c2) = Conref (Set.union c1 c2)
(|&|) EQV _ = EQV
(|&|) _ EQV = EQV

-- | Implication for conrefs.
impl :: (CARD s) => Conref s -> Conref s -> Bool
impl (Conref c1) (Conref c2) = and (Set.map (`Set.member` c1) c2)
impl EQV _ = True
impl (Conref _) EQV = False

-- | Check that first effect is less than or equal to second effect,
-- according to order defined by conref.
checkLe :: (CARD s) => Conref s -> Effect s -> Effect s -> Bool
checkLe (Conref cs) e1 e2 = 
  and (defineLe <$> (Set.toList cs) <*> pure e1 <*> pure e2)

-- | Check if conref blocks effect, returning 'True' when it does
-- block.
checkBlock :: (CARD s) => Conref s -> Effect s -> Bool
checkBlock (Conref cs) (Effect es) = or (defineConflict <$> (Set.toList cs) <*> es)
checkBlock EQV (Effect es) = case es of
                               [] -> False
                               _ -> True

-- | Check if conref blocks effect, returning the 'Left' value if
-- there is a block.
checkBlock' :: (CARD s) => Conref s -> Effect s -> Either (Conref s) ()
checkBlock' (Conref cs) (Effect es) = 
  if or (defineConflict <$> (Set.toList cs) <*> es)
     then Left (Conref cs)
     else Right ()
checkBlock' EQV (Effect es) = case es of
                                [] -> Right ()
                                _ -> Left EQV

------------------------------------------------------------------------
-- Simple implementations

newtype Counter = Counter Int deriving (Show,Read,Eq,Ord,Generic)

instance Semigroup Counter where
  (<>) (Counter a) (Counter b) = Counter (a + b)
instance Monoid Counter where
  mempty = Counter 0

instance CARD Counter where
  data Ef Counter = Add Int | Sub Int | SetTo Int deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (Counter s) e = case e of
    Add n -> Counter (s + n)
    Sub n -> Counter (s - n)
    SetTo n -> Counter n

  data Cr Counter = LEQ | GEQ deriving (Show,Read,Eq,Ord,Generic)
  defineConflict c e = case (c,e) of
    (LEQ,Sub _) -> True
    (GEQ,Add _) -> True
    (_,SetTo _) -> True
    _ -> False
  defineLe c (Effect es1) (Effect es2) = 
    let f e a = case e of
                  Add n -> a + n
                  Sub n -> a - n
        v1 = foldr f 0 es1
        v2 = foldr f 0 es2
    in case c of
         LEQ -> v1 <= v2
         GEQ -> v1 >= v2

instance ToJSON Counter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Counter where
instance ToJSON (Ef Counter) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Ef Counter)
instance ToJSON (Cr Counter) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (Cr Counter)

newtype RGArray a = RGArray [a] deriving (Show,Read,Eq,Ord,Generic)

instance Semigroup (RGArray a) where
  (<>) (RGArray a) (RGArray b) = RGArray (a <> b)
instance Monoid (RGArray a) where
  mempty = RGArray mempty

instance (Eq a, Ord a) => CARD (RGArray a) where
  data Ef (RGArray a) = RGAppend a
                      | RGRemove a
                      deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (RGArray as) (RGAppend a) = RGArray (as ++ [a])
  defineEffect (RGArray as) (RGRemove a) = RGArray (List.delete a as)

  data Cr (RGArray a) = CrRGArray deriving (Show,Read,Eq,Ord,Generic)
  defineConflict _ _ = False

instance ToJSON a => ToJSON (RGArray a) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (RGArray a)
instance ToJSON a => ToJSON (Ef (RGArray a)) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Ef (RGArray a))
instance ToJSON a => ToJSON (Cr (RGArray a)) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (Cr (RGArray a))
