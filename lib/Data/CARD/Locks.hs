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

data Token i = Token
  { tkSeqNum :: Int
  , tkOwner :: i
  , tkState :: Maybe (Set i)
  , tkRequests :: Set i }
  deriving (Eq,Ord,Generic)

deriving instance (Show i) => Show (Token i)

nextId :: (Ord i) => i -> Set i -> Maybe i
nextId i ids =
  let ids' = Set.filter (> i) ids
  in if Set.null ids'
        then Set.lookupMin ids -- wrap-around case
        else Set.lookupMin ids'

initToken :: (Ord i) => i -> Token i
initToken i = Token 0 i (Just (Set.singleton i)) mempty

-- | Either pass token to next owner, or release lock
passToken :: (Ord i) => Token i -> Token i
passToken t@(Token n i s rs) =
  case nextId i rs of
    Just i' -> Token (n + 1) i' s (Set.delete i' rs)
    Nothing -> Token n i Nothing rs

requestToken :: (Ord i) => i -> Token i -> Token i
requestToken i (Token n o Nothing rs) | i == o = 
  Token (n + 1) o (Just (Set.singleton i)) rs
requestToken i (Token n o s rs) | i /= o = Token n o s (Set.insert i rs)
requestToken i t = t

instance (ToJSON i) => ToJSON (Token i)
instance (Ord i, FromJSONKey i, FromJSON i) => FromJSON (Token i)

instance (Ord i) => Semigroup (Token i) where
  t1 <> t2 | tkSeqNum t1 < tkSeqNum t2 = t2
  t1 <> t2 | tkSeqNum t1 > tkSeqNum t2 = t1
  Token n i s1 rs1 <> Token _ _ s2 rs2 = Token n i s' (rs1 <> rs2)
    where s' = case (s1,s2) of
                 (Just ss1, Just ss2) -> Just $ ss1 <> ss2
                 _ -> Nothing

data Locks i s =
    Locks { locks :: Map i (Int, Conref s, Set i) }
  | Tokens { tokens :: Map (Cr s) (Token i)}
  deriving (Eq,Ord,Generic)

deriving instance (Show i, Show (Cr s)) => Show (Locks i s)

instance (ToJSONKey i, ToJSON i, ToJSONKey (Cr s), ToJSON (Cr s)) => ToJSON (Locks i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Cr s), FromJSONKey (Cr s), FromJSON (Cr s)) => FromJSON (Locks i s)

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
  (<>) (Tokens ts1) (Tokens ts2) =
    Tokens $ foldl' (\m k -> case (Map.lookup k ts1, Map.lookup k ts2) of
      (Just t1, Just t2) -> Map.insert k (t1 <> t2) m
      (Just t1, Nothing) -> Map.insert k t1 m
      (Nothing, Just t2) -> Map.insert k t2 m)
                    mempty (nub $ Map.keys ts1 ++ Map.keys ts2)
  (<>) (Tokens ts) _ = Tokens ts
  (<>) _ (Tokens ts) = Tokens ts

instance (Ord i, CARD s) => Monoid (Locks i s) where
  mempty = Locks (mempty)

instance (Ord i, CARD s, Monad m) => CvRDT r (Locks i s) m where
  cvmerge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty

initTokens :: (Ord i, CARD s) => [Cr s] -> i -> Locks i s
initTokens cs i = Tokens $ Map.fromList (zip cs (repeat $ initToken i))

-- | Get the current request index for an identity
reqIndex :: (Ord i, CARD s) => i -> Locks i s -> Int
reqIndex i (Locks m) = case Map.lookup i m of
                         Just (n,_,_) -> n
                         Nothing -> 0

-- | Request a new/stronger lock
request :: (Ord i, CARD s) => i -> Conref s -> Locks i s -> Locks i s
request i (Conref cs) (Tokens ts) = 
  Tokens $ foldr (Map.adjust (requestToken i)) ts cs
request i c m = m <> (Locks $ Map.singleton i (reqIndex i m, c, mempty))

-- | Grant a request
grant :: (Ord i, CARD s) 
      => i -- ^ Granting identity
      -> i -- ^ Identity whose request is being granted
      -> Locks i s 
      -> Locks i s
grant ig ir (Tokens ts) = Tokens $ Map.map f ts
  where f (Token n o (Just ss) rs) | ir == o = 
                                     Token n o (Just (Set.insert ig ss)) rs
        f t = t
grant ig ir (Locks m) = 
  Locks $ Map.adjust (\(n,c,s) -> if c /= crT
                                     then (n,c,Set.insert ig s)
                                     else (n,c,s)) ir m

-- | Release a lock
release :: (Ord i, CARD s) => i -> Locks i s -> Locks i s
release i (Tokens ts) = Tokens $ Map.map f ts
  where f t = if tkOwner t == i
                 then passToken t
                 else t
release i m = m <> (Locks $ Map.singleton i (reqIndex i m + 1, crT, mempty))

-- | List all requests that 'i' has not granted
ungranted :: (Ord i, CARD s) => i -> Locks i s -> [(i,Conref s)]
ungranted i (Tokens ts) = Map.foldrWithKey f [] ts
  where f c (Token _ o (Just ss) rs) cs = if Set.member i ss
                                             then cs
                                             else (o,cr c) : cs
        f _ _ cs = cs
ungranted i (Locks m) = map (\(ir,(_,c,_)) -> (ir,c)) $ filter ug (Map.assocs m)
  where ug (ir,((_,c,s))) = i /= ir && not (i `Set.member` s) && c /= crT

-- -- | Check if effect is blocked by current locks (as compared to a
-- -- consumed effect)
-- permitted :: (Ord i, CARD s) 
--   => i
--      -- | The consumed effect
--   -> Effect s
--      -- | The issued/produced effect
--   -> Effect s
--   -> Locks i s
--   -> Bool
-- permitted i ce ie =
--   not
--   . or 
--   . map (\(n,c,s) -> not (checkLe c ce ie) && Set.member i s)
--   . Map.elems
--   . locks

-- | Check if effect is blocked by current locks, returning the
-- blocking 'Conref' if so (as compared with consumed effect)
permitted' :: (Ord i, CARD s)
  => i
  -> Effect s -- ^ The consumed effect
  -> Effect s -- ^ The issued/produced effect
  -> Locks i s
  -> Either (Conref s) ()
permitted' i ce ie (Tokens ts) =
  let f (c, (Token _ o (Just ss) _)) = not (checkLe (cr c) ce ie) 
                                       && Set.member i ss
                                       && o /= i
      blockers = filter f $ Map.assocs ts
  in case blockers of
       [] -> Right ()
       bs -> Left (fold (map (cr.fst) bs))
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
requested i (Conref cs) (Tokens ts) = and (map f (Set.toList cs))
  where f c = case Map.lookup c ts of
                Just (Token _ o (Just _) _) | i == o -> True
                Just (Token _ o _ rs) | i /= o && Set.member i rs -> True
                _ -> False
requested i c1 (Locks m) = case Map.lookup i m of
  Just (_,c2,_) -> c2 `impl` c1
  Nothing -> if c1 == crT
                then True
                else False

-- | Check if node 'i' is holding any locks
holding :: (Ord i, CARD s) => i -> Locks i s -> Bool
holding i (Tokens ts) = case Map.lookup i m of
                          _ -> undefined
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
