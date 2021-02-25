{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  { resStore :: Map (EResId i) (ERes s) }
  deriving (Eq,Ord,Generic)

deriving instance (CARD s, Show i, Show (Effect s)) => Show (Ress i s)

type EResId i = (i,i,Int)

-- | Get the id of the replica that can consume this reservation
owner :: Lens' (EResId i) i
owner = _1

-- | Get the id of the replica that has produced this reservation
producer :: Lens' (EResId i) i
producer = _2

-- | Get the sequence number of this reservation
seqNum :: Lens' (EResId i) Int
seqNum = _3

type ERes s = (Int, Effect s)

erClock :: Lens' (ERes s) Int
erClock = _1

erEffect :: (CARD s) => Lens' (ERes s) (Effect s)
erEffect = _2

newERes :: (CARD s) => Effect s -> ERes s
newERes e = (0, e)

-- | Replace a reservation's effect with a new, partly or wholly
-- consumed, effect.  This automatically bumps the clock.
modERes :: (CARD s) => Effect s -> ERes s -> ERes s
modERes e (n,_) = (n + 1,e)

instance (ToJSONKey i, ToJSON i, ToJSON (Ef s)) => ToJSON (Ress i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Ef s), FromJSON (Ef s)) 
         => FromJSON (Ress i s)

highestSeqNum :: (Eq i) => i -> [EResId i] -> Int
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
  in Ress $ foldr (\(n,(o,e)) a -> Map.insert (o,i,n) (newERes e) a) 
                  m
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
consumeRes _ e r | e == ef0 = Just r
consumeRes i e (Ress m) =
  let fr ((k,r):es) | k^.owner == i && (r^.erEffect) == e = Just (k,ef0)
      fr ((k,r):es) | k^.owner == i = case extractE (r^.erEffect) e of
                                        Right e' -> Just (k,e')
                                        Left () -> fr es
      fr (_:es) = fr es
      fr [] = Nothing
  in case fr (Map.assocs m) of
       Just (k,e') -> Just (Ress (Map.adjust (modERes e') k m))
       Nothing -> Nothing

-- | Look up effect reservations owned by a replica.
resLookup :: (Ord i, CARD s) => i -> Ress i s -> [Effect s]
resLookup i (Ress m) = view erEffect <$> Map.foldrWithKey' f [] m
  where f k r rs = if k^.owner == i
                      then r : rs
                      else rs

resKeys (Ress m) = Map.keys m

-- | Gather all effect reservations.  Note that this will arbitrarily
-- order effects by owning replica.
resAll :: (Ord i, CARD s) => Ress i s -> [Effect s]
resAll (Ress m) = view erEffect <$> Map.elems m

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
                (Just (n1,e1), Just (n2,_)) | n1 >= n2 -> 
                                              Map.insert k (n1,e1)
                (Just r, Nothing) -> Map.insert k r
                (_, Just r) -> Map.insert k r
                _ -> error "Uh oh."
    in Ress $ foldr f mempty ks

instance (Ord i, CARD s) => Monoid (Ress i s) where
  mempty = Ress mempty

instance (Ord i, CARD s, Monad m) => CvRDT r (Ress i s) m where
  cvmerge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty
