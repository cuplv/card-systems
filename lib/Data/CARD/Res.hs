{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Res
  ( Ress
  , produceRes
  , consumeRes
  , resLookup
  , resAll
  , resWX
  ) where

import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import GHC.Generics hiding (Rep)
import Data.Aeson
import Data.List (nub)
import Control.Lens

import Data.CvRDT
import Data.CARD

-- | Reservation store
data Ress i s = Ress
  { resStore :: Map (i,i,Int) (Maybe (Effect s))}
  deriving (Eq,Ord,Generic)

type ResId i = (i,i,Int)

-- | Get the id of the replica that can consume this reservation
owner :: Lens' (ResId i) i
owner = _1

-- | Get the id of the replica that has produced this reservation
producer :: Lens' (ResId i) i
producer = _2

-- | Get the sequence number of this reservation
seqNum :: Lens' (ResId i) Int
seqNum = _3

instance (ToJSONKey i, ToJSON i, ToJSON (Ef s)) => ToJSON (Ress i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Ef s), FromJSON (Ef s)) 
         => FromJSON (Ress i s)

highestSeqNum :: (Eq i) => i -> [ResId i] -> Int
highestSeqNum i = foldr max 0
                  . map (view seqNum)
                  . filter (\k -> k^.producer == i)

-- | Produce a sequence of effect reservations.  The first replica ID
-- argument is the producing replica, and the reservations are pairs
-- of the form (Owner,Effect), where the Owner is the replica that
-- will be able to consume this effect.
produceRes :: (Ord i, CARD s) => i -> [(i,Effect s)] -> Ress i s -> Ress i s
produceRes i es (Ress m) =
  let n0 = highestSeqNum i (Map.keys m)
  in Ress $ foldr (\(n,(o,e)) a -> Map.insert (o,i,n) (Just e) a) 
                  mempty
                  (zip [(n0 + 1) ..] es)

-- | Consume an effect from a reservation store.  The first argument
-- is the ID of the replica that is doing the consuming.  If the
-- effect can be removed, the result is Just (RemainingResStore).
-- Otherwise, the result is Nothing.
consumeRes :: (Ord i, CARD s) 
  => i
  -> Effect s
  -> Ress i s
  -> Maybe (Ress i s)
consumeRes i e (Ress m) =
  let fr ((k,Just e'):es) | k^.owner == i && e' == e = Just k
      fr (a:es) = fr es
      fr [] = Nothing
  in case fr (Map.assocs m) of
       Just k -> Just (Ress (Map.delete k m))
       Nothing -> Nothing

-- | Look up effect reservations owned by a replica.
resLookup :: (Ord i, CARD s) => i -> Ress i s -> [Effect s]
resLookup i (Ress m) = catMaybes $ Map.foldrWithKey' f [] m
  where f k e es = if k^.owner == i
                      then e : es
                      else es

resKeys (Ress m) = Map.keys m

-- | Gather all effect reservations.  Note that this will arbitrarily
-- order effects by owning replica.
resAll :: (Ord i, CARD s) => Ress i s -> [Effect s]
resAll (Ress m) = catMaybes $ Map.elems m

-- | Get the worst-case effect from the reservation store.  This
-- simply filters out effects that are in accord with the specified
-- conref.
resWX :: (Ord i, CARD s) => Conref s -> Ress i s -> Effect s
resWX c _ | c == crT = ef0 -- optimization case
resWX c r = 
  let f e es = if checkBlock c e 
                  then e |>>| es
                  else es
  in foldr f ef0 (resAll r)

instance (Ord i, CARD s) => Semigroup (Ress i s) where
  (<>) (Ress m1) (Ress m2) =
    let ks = nub $ Map.keys m1 ++ Map.keys m2
        f k = case (Map.lookup k m1, Map.lookup k m2) of
                (Just Nothing, _) -> Map.insert k Nothing
                (_, Just Nothing) -> Map.insert k Nothing
                (Just e, _) -> Map.insert k e
                (_, Just e) -> Map.insert k e
    in Ress $ foldr f mempty ks

instance (Ord i, CARD s) => Monoid (Ress i s) where
  mempty = Ress mempty

instance (Ord i, CARD s, Monad m) => CvRDT r (Ress i s) m where
  cvmerge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty
