{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Discard.Experiment2
  ( Exp2Conf (..)
  ) where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.Time.Clock

import Lang.Carol
import Network.Discard.Experiment

data Exp2Conf = Exp2Conf
  { e2UseReservations :: Bool
  , e2Time :: Int
  , e2Restocker :: Bool
  , e2WarehouseSize :: Int
  }
  deriving (Eq,Ord,Show,Generic)

instance ToJSON Exp2Conf where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Exp2Conf
