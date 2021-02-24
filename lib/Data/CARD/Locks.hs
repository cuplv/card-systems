{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.CARD.Locks
  ( Locks
  -- * Modify the locks
  , request
  , grant
  , release
  -- * Examine the locks
  , permitted
  , permitted'
  , requested
  , ungranted
  , holding
  , holding'
  , confirmed
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import GHC.Generics hiding (Rep)
import Data.Aeson
import Data.List (nub)
import Data.Foldable (fold,foldl')

import Control.Monad.Free

import Data.CvRDT
import Data.CARD

data Locks i s = Locks 
  { locks :: Map i (Int, Conref s, Set i) } 
  deriving (Eq,Ord,Generic)

instance (ToJSONKey i, ToJSON i, ToJSON (Cr s)) => ToJSON (Locks i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Cr s), FromJSON (Cr s)) => FromJSON (Locks i s)

instance (Ord i, CARD s) => Semigroup (Locks i s) where
  (<>) (Locks m1) (Locks m2) = 
    Locks $ foldl' (\m k -> case (Map.lookup k m1, Map.lookup k m2) of
      (Just (i1,c1,s1),Just (i2,c2,s2)) -> case compare i1 i2 of
        EQ | c1 == c2 -> Map.insert k (i1, c1, s1 `Set.union` s2) m
           | c1 `impl` c2 -> Map.insert k (i1, c1, s1) m
           | c2 `impl` c1 -> Map.insert k (i1, c2, s2) m
           | otherwise -> Map.insert k (i1, c1 |&| c2, Set.empty) m
        LT -> Map.insert k (i2,c2,s2) m
        GT -> Map.insert k (i1,c1,s1) m
      (Just v1,Nothing) -> Map.insert k v1 m
      (Nothing,Just v2) -> Map.insert k v2 m) 
                   mempty (nub $ Map.keys m1 ++ Map.keys m2)

instance (Ord i, CARD s) => Monoid (Locks i s) where
  mempty = Locks (mempty)

instance (Ord i, CARD s, Monad m) => CvRDT r (Locks i s) m where
  cvmerge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty

-- | Get the current request index for an identity
reqIndex :: (Ord i, CARD s) => i -> Locks i s -> Int
reqIndex i (Locks m) = case Map.lookup i m of
                         Just (n,_,_) -> n
                         Nothing -> 0

-- | Request a new/stronger lock
request :: (Ord i, CARD s) => i -> Conref s -> Locks i s -> Locks i s
request i c m = m <> (Locks $ Map.singleton i (reqIndex i m, c, mempty))

-- | Grant a request
grant :: (Ord i, CARD s) 
      => i -- ^ Granting identity
      -> i -- ^ Identity whose request is being granted
      -> Locks i s 
      -> Locks i s
grant ig ir (Locks m) = 
  Locks $ Map.adjust (\(n,c,s) -> if c /= crT
                                     then (n,c,Set.insert ig s)
                                     else (n,c,s)) ir m

-- | Release a lock
release :: (Ord i, CARD s) => i -> Locks i s -> Locks i s
release i m = m <> (Locks $ Map.singleton i (reqIndex i m + 1, crT, mempty))

-- | List all requests that 'i' has not granted
ungranted :: (Ord i, CARD s) => i -> Locks i s -> [(i,Conref s)]
ungranted i (Locks m) = map (\(ir,(_,c,_)) -> (ir,c)) $ filter ug (Map.assocs m)
  where ug (ir,((_,c,s))) = i /= ir && not (i `Set.member` s) && c /= crT

-- | Check if effect is blocked by current locks (as compared to a
-- consumed effect)
permitted :: (Ord i, CARD s) 
  => i
     -- | The consumed effect
  -> Effect s
     -- | The issued/produced effect
  -> Effect s
  -> Locks i s
  -> Bool
permitted i ce ie =
  not
  . or 
  . map (\(n,c,s) -> checkLe c ce ie && Set.member i s)
  . Map.elems
  . locks

-- | Check if effect is blocked by current locks, returning the
-- blocking 'Conref' if so (as compared with consumed effect)
permitted' :: (Ord i, CARD s)
  => i
     -- | The consumed effect
  -> Effect s
     -- | The issued/produced effect
  -> Effect s
  -> Locks i s
  -> Either (Conref s) ()
permitted' i ce ie ls =
  let blockers = map (\(_,c,_) -> c)
                 . filter (\(n,c,s) -> not (checkLe c ce ie)
                                       && Set.member i s)
                 . Map.elems
                 . locks
                 $ ls
  in case blockers of
       [] -> Right ()
       bs -> Left (fold bs)

-- | Check if lock has been requested
requested :: (Ord i, CARD s) => i -> Conref s -> Locks i s -> Bool
requested i c1 (Locks m) = case Map.lookup i m of
  Just (_,c2,_) -> c2 `impl` c1
  Nothing -> if c1 == crT
                then True
                else False

-- | Check if node 'i' is holding any locks
holding :: (Ord i, CARD s) => i -> Locks i s -> Bool
holding i (Locks m) = case Map.lookup i m of
  Just (_,c,_) -> if c == crT
                     then False
                     else True
  Nothing -> False

-- | Return exactly the lock that 'i' is holding
holding' :: (Ord i, CARD s) => i -> Locks i s -> Conref s
holding' i (Locks m) = case Map.lookup i m of
  Just (_,c,_) -> c
  Nothing -> crT

-- | Check if currently requested lock has been confirmed by all
-- participating identities
confirmed :: (Ord i, CARD s) 
          => i -- ^ Identity to check the requested lock of
          -> [i] -- ^ List of other participating identities
          -> Locks i s 
          -> Bool
confirmed self others (Locks m) = 
  case Map.lookup self m of
    Just (_,c,s) -> 
      c == crT 
      || Set.fromList others `Set.isSubsetOf` s
    Nothing -> True
