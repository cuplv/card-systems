{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.CARD.Res
  ( Res
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics hiding (Rep)
import Data.Aeson
import Data.List (nub)

import Data.CvRDT
import Data.CARD

data Res i s = Res
  { resStore :: Map i (Effect s)}
  deriving (Eq,Ord,Generic)

instance (ToJSONKey i, ToJSON i, ToJSON (Ef s)) => ToJSON (Res i s)
instance (Ord i, FromJSONKey i, FromJSON i, Ord (Ef s), FromJSON (Ef s)) => FromJSON (Res i s)

resLookup :: (Ord i, CARD s) => i -> Res i s -> Effect s
resLookup i (Res m) = case Map.lookup i m of
  Just e -> e
  Nothing -> ef0

resKeys (Res m) = Map.keys m

instance (Ord i, CARD s) => Semigroup (Res i s) where
  (<>) r1 r2 =
    let ks = nub $ resKeys r1 ++ resKeys r2
    in undefined
