{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Discard.Experiment2
  ( Exp2Conf (..)
  , Exp2Result (..)
  , ExpCmd (..)
  , getTime
  , getBatchSize
  , getUseTokens
  ) where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.Time.Clock

import Lang.Carol
import Network.Discard.Broadcast
import Network.Discard.Experiment

getBatchSize (Left e) = expBatchSize e
getBatchSize (Right e) = e2BatchSize e

data Exp2Conf = Exp2Conf
  { e2UseReservations :: Bool
  , e2Time :: Int
  , e2Restocker :: Bool
  , e2WarehouseSize :: Int
  , e2Rate :: Int
  , e2BatchSize :: Int
  , e2UseTokens :: Bool
  }
  deriving (Eq,Ord,Show,Generic)

instance ToJSON Exp2Conf where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Exp2Conf

data Exp2Result = Exp2Result
  { e2TotalSales :: Int
  } deriving (Show,Generic)

instance ToJSON Exp2Result where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Exp2Result

instance Semigroup Exp2Result where
  Exp2Result n <> Exp2Result m = Exp2Result (n + m)

instance Monoid Exp2Result where
  mempty = Exp2Result 0

data ExpCmd i = Launch (Either ExpConf Exp2Conf) (NetConf i)
              | Report Int
              deriving (Eq,Ord,Show,Generic)

instance (ToJSON i) => ToJSON (ExpCmd i) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord i, FromJSON i) => FromJSON (ExpCmd i)

getTime (Right e) = e2Time e
getTime (Left e) = expTime e

getUseTokens (Right e) = e2UseTokens e
getUseTokens (Left e) = expUseTokens e
