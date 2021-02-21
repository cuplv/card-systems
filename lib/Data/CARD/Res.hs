{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CARD.Res
  ( Ress (..)
  , resLookup
  , resAll
  , resWX
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics hiding (Rep)
import Data.Aeson
import Data.List (nub)

import Data.CvRDT
import Data.CARD

data Ress i s = Ress
  { resStore :: Map i (Effect s)}
  deriving (Eq,Ord,Generic)

instance (ToJSONKey i, ToJSON i, ToJSON (Ef s)) => ToJSON (Ress i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Ef s), FromJSON (Ef s)) 
         => FromJSON (Ress i s)

-- | Look up effect reservations owned by a replica.
resLookup :: (Ord i, CARD s) => i -> Ress i s -> Effect s
resLookup i (Ress m) = case Map.lookup i m of
  Just e -> e
  Nothing -> ef0

resKeys (Ress m) = Map.keys m

-- | Combine all effect reservations into a single effect.  Note that
-- this will arbitrarily order effects by owning replica.
resAll :: (Ord i, CARD s) => Ress i s -> Effect s
resAll (Ress m) = Map.foldr (|>>|) ef0 m

-- | Get the worst-case effect from the reservation store.  This
-- simply filters out effects that are in accord with the specified
-- conref.
resWX :: (Ord i, CARD s) => Conref s -> Ress i s -> Effect s
resWX c _ | c == crT = ef0 -- optimization case
resWX c (Ress m) = 
  let f e es = if checkBlock c e 
                  then e |>>| es
                  else es
  in Map.foldr f ef0 m

instance (Ord i, CARD s) => Semigroup (Ress i s) where
  (<>) r1 r2 =
    let ks = nub $ resKeys r1 ++ resKeys r2
        f k = Map.insert k (resLookup k r1 |>>| resLookup k r2)
    in Ress $ foldr f mempty ks

instance (Ord i, CARD s) => Monoid (Ress i s) where
  mempty = Ress mempty

instance (Ord i, CARD s, Monad m) => CvRDT r (Ress i s) m where
  cvmerge _ s1 s2 = pure (s1 <> s2)
  cvempty _ = pure mempty
