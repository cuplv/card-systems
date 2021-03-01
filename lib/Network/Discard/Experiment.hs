{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Discard.Experiment
  ( defaultServerPort
  , defaultBatchSize
  , ExpConf (..)
  , divRate
  , ExpNetConf (..)
  , getNetConf
  , expAddrs
  , Mix (..)
  , Profile
  , chooseOp
  , ExpResult (..)
  , totalLats
  , emptyResults
  , combineResults
  , totalReqs
  , trueReqRate
  , combinedAvgLatency

  ) where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.Time.Clock

import Lang.Carol
import Network.Discard.Broadcast

-- expTime is a number of seconds
data ExpConf = ExpConf 
  { expRate :: Int
  , expApp :: String
  , expMix :: Mix
  , expTime :: Int
  , expBatchSize :: Int }
  deriving (Eq,Ord,Show,Generic)

instance ToJSON ExpConf where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ExpConf

-- | Divide request rate by number of hosts, so that all hosts
-- together will perform the original rate
divRate :: ExpNetConf i -> ExpConf -> ExpConf
divRate (ExpNetConf nc) (ExpConf r a m t b) = 
  ExpConf (r `div` Map.size nc) a m t b

data Mix = Mix [(Int,String)] String deriving (Eq,Ord,Show,Generic)

instance ToJSON Mix where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Mix

type Profile s = String -> Carol s ()

chooseOp :: Profile s -> Mix -> Int -> (String, Carol s ())
chooseOp prf (Mix ns df) n = 
  let opStr = foldr (\(n',s') s -> if n <= n'
                                      then s'
                                      else s) df ns
  in (opStr, prf opStr)

data ExpResult = ExpResult 
  { expAvgLatencies :: Map String NominalDiffTime
  , expReqCounts :: Map String Int
  , expUnfinished :: Map String Int
  } deriving (Show,Generic)

instance ToJSON ExpResult where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ExpResult

-- | Map of 'String's to the total latency time spent on their kind of
-- request
totalLats :: ExpResult -> Map String NominalDiffTime
totalLats (ExpResult lats cs _) = 
  Map.mapWithKey (\s l -> l * fromIntegral (cs Map.! s)) lats

-- | Total latency time spent across all requests
combinedLats :: ExpResult -> NominalDiffTime
combinedLats = foldr (+) 0 . totalLats

emptyResults :: ExpResult
emptyResults = ExpResult Map.empty Map.empty Map.empty

combineResults :: ExpResult -> ExpResult -> ExpResult
combineResults e1@(ExpResult lats1 cs1 unf1) e2@(ExpResult lats2 cs2 unf2) = 
  let cs = Map.unionWith (+) cs1 cs2
      tots = Map.unionWith (+) (totalLats e1) (totalLats e2)
      lats = Map.mapWithKey (\s l -> l / fromIntegral (cs Map.! s)) tots
      unf = Map.unionWith (+) unf1 unf2
  in ExpResult lats cs unf

totalReqs :: ExpResult -> Int
totalReqs = foldr (+) 0 . expReqCounts

combinedAvgLatency :: ExpResult -> NominalDiffTime
combinedAvgLatency e = combinedLats e / (fromRational . fromIntegral $ totalReqs e)

trueReqRate :: ExpConf
            -> ExpResult 
            -> Int
trueReqRate conf res = totalReqs res `div` expTime conf

instance Semigroup ExpResult where
  (<>) = combineResults
instance Monoid ExpResult where
  mempty = emptyResults

data ExpNetConf i = ExpNetConf (Map i (String, Int, Int)) deriving (Show,Eq,Ord)

getNetConf :: ExpNetConf i -> NetConf i
getNetConf (ExpNetConf m) = NetConf (fmap (\(n,p,_) -> (n,p)) m)

expAddrs :: ExpNetConf i -> Map i String
expAddrs (ExpNetConf m) = fmap (\(h,_,p) -> "http://" ++ h ++ ":" ++ show p) m

instance (ToJSON i) => ToJSON (ExpNetConf i) where
  toJSON (ExpNetConf m) = 
    toJSON (map ent $ Map.assocs m)
    where ent (i,(h,p,c)) = object ["name" .= i
                                   ,"host" .= h
                                   ,"port" .= p
                                   ,"expPort" .= c]

instance (Ord i, FromJSON i) => FromJSON (ExpNetConf i) where
  parseJSON = withArray "ExpNodeList" $ \v -> ExpNetConf <$> do
    foldM unpack Map.empty v
    where unpack m = withObject "Node" $ \v -> do
            name <- v .: "name"
            host <- v .:? "host" .!= "localhost"
            port <- v .:? "port" .!= defaultPort
            expPort <- v .:? "expPort" .!= defaultServerPort
            return (Map.insert name (host,port,expPort) m)

defaultServerPort = 24001 :: Int

defaultBatchSize = 50 :: Int
