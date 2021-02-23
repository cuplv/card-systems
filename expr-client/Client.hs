{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.HTTP.Types
import Network.HTTP.Client
import Data.Yaml (decodeFileEither)
import Data.Aeson (encode,decode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BC
import Options.Applicative
import Control.Concurrent.STM
import Data.Map (Map)
import System.IO (hFlush,stdout)
import qualified Data.Map as Map
import System.Random
import Data.Time.Clock
import Control.Concurrent (threadDelay,forkIO)
import Data.List (genericLength)
import System.Exit
import Data.Foldable (fold)

import Lang.Carol
import Lang.Carol.Bank
import Network.Discard
import Network.Discard.Experiment
import Data.EventGraph.Ipfs

main :: IO ()
main = do 
  conf <- confCLI
  net <- decodeFileEither (clientNetConf conf) >>= \case
    Right net -> return net
    Left _ -> die "Could not read net conf file"
  runExperiment 
    (ExpConf (clientRate conf) "asdf" (uncurry Mix (clientMix conf)) (clientTime conf))
    net

data ClientConf i = ClientConf
  { clientNetConf :: FilePath
  , clientMix :: ([(Int,String)],String)
  , clientMixSpecial :: ([(Int,String)],String)
  , clientRate :: Int
  , clientTime :: Int }

confCLI :: IO (ClientConf String)
confCLI = execParser $
  let parser = ClientConf
        <$> strOption (short 'c' <> help "net conf filepath")
        <*> option auto (long "mix")
        <*> option auto (long "special-mix" <> help "mix for a single \"special\" node to use")
        <*> option auto (long "rate" <> metavar "MICROSEC")
        <*> option auto (long "time" <> metavar "SEC")
      misc = (fullDesc <> progDesc "Run an experiment")
  in info (parser <**> helper) misc

runExperiment :: ExpConf -> ExpNetConf String -> IO ()
runExperiment ec enc = do
  let nc = getNetConf enc
      addrs = expAddrs enc
      cmd = Launch (divRate enc ec) nc
  man <- newManager defaultManagerSettings
  let mkReq :: String -> IO (Int,String)
      mkReq addr = do
        ireq <- parseRequest addr
        let req = ireq { requestBody = RequestBodyLBS $ encode cmd }
        resp <- httpLbs req man
        case responseStatus $ resp of
          c | c == status200 -> case decode (responseBody resp) of
                                  Just n -> return (n,addr)
          c -> print c >> die "Oops"
  exps <- traverse mkReq addrs
  putStrLn "All nodes started experiment." >> hFlush stdout
  threadDelay $ (expTime ec + 15) * 1000000
  let getRes :: (Int,String) -> IO ExpResult
      getRes (n,addr) = do
        ireq <- parseRequest addr
        let req = ireq { requestBody = RequestBodyLBS $ encode (Report n :: ExpCmd String) }
        resp <- httpLbs req man
        case responseStatus $ resp of
          c | c == status200 -> case decode (responseBody resp) of
                                  Just d -> return d
            | otherwise -> print c >> die "Oops"
  results <- fold <$> traverse getRes exps
  putStrLn $ "* Individual latencies"
  mapM_ (\(s,l) -> putStrLn $ s ++ ": " ++ show l ++ " s") (Map.assocs (expAvgLatencies results))
  putStrLn $ "* Failures"
  print $ expUnfinished results
  putStrLn $ "* Total measures"
  putStrLn $ "Total requests: " ++ show (totalReqs results) ++ " req"
  putStrLn $ "True rate: " ++ show (trueReqRate ec results) ++ " req/s"
  putStrLn $ "Avg latency: " ++ show (combinedAvgLatency results) ++ " s"
