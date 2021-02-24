{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Discard.Experiment2
  ( Exp2Conf (..)
  , Exp2Result (..)
  , ExpCmd (..)
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

data Exp2Conf = Exp2Conf
  { e2UseReservations :: Bool
  , e2Time :: Int
  , e2Restocker :: Bool
  , e2WarehouseSize :: Int
  , e2Rate :: Int
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

data ExpCmd i = Launch (Either ExpConf Exp2Conf) (NetConf i)
              | Report Int
              deriving (Eq,Ord,Show,Generic)

instance (ToJSON i) => ToJSON (ExpCmd i) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord i, FromJSON i) => FromJSON (ExpCmd i)
