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
import Network.Discard.Experiment2
import Data.EventGraph.Ipfs

main :: IO ()
main = do 
  conf <- execParser (info (subparser (command "latency" conf1CLI 
                                       <> command "stock" conf2CLI)
                            <**> helper) (fullDesc <> progDesc "Experiment client"))
  net <- decodeFileEither (clientNetConf conf) >>= \case
    Right net -> return net
    Left _ -> die "Could not read net conf file"
  case conf of
    Client1Conf _ _ _ _ -> runExperiment 
                             (Left $ ExpConf (clientRate conf) "asdf" (uncurry Mix (clientMix conf)) (clientTime conf))
                             net
    Client2Conf _ _ _ _ _ -> runExperiment
                              (Right $ Exp2Conf (clientUseRes conf) (clientTime conf) False (clientWarehouseSize conf) (clientRate conf))
                              net

data ClientConf i = 
    Client1Conf
      { clientNetConf :: FilePath
      , clientMix :: ([(Int,String)],String)
      , clientRate :: Int
      , clientTime :: Int
      }
  | Client2Conf
      { clientNetConf :: FilePath
      , clientWarehouseSize :: Int
      , clientRate :: Int
      , clientTime :: Int
      , clientUseRes :: Bool
      }

-- conf1CLI :: IO (ClientConf String)
conf1CLI =
  let parser = Client1Conf
        <$> strOption (short 'c' <> help "net conf filepath")
        <*> option auto (long "mix")
        <*> option auto (long "rate" <> metavar "MICROSEC")
        <*> option auto (long "time" <> metavar "SEC")
      misc = (fullDesc <> progDesc "Run an experiment")
  in info (parser <**> helper) misc

-- conf2CLI :: IO (ClientConf String)
conf2CLI =
  let parser = Client2Conf
        <$> strOption (short 'c' <> help "net conf filepath")
        <*> option auto (long "size")
        <*> option auto (long "rate" <> metavar "MICROSEC")
        <*> option auto (long "time" <> metavar "SEC")
        <*> switch (long "res" <> help "use reservations")
      misc = (fullDesc <> progDesc "Run an experiment")
  in info (parser <**> helper) misc

runExperiment :: Either ExpConf Exp2Conf -> ExpNetConf String -> IO ()
runExperiment ec enc = do
  let nc = getNetConf enc
      addrs = expAddrs enc
      -- cmd = Launch (divRate enc ec) nc
      cmd = undefined
      cmds = case ec of
               Left e1c -> repeat $ Launch (Left $ divRate enc e1c) nc
               Right e2c -> Launch (Right $ e2c { e2Restocker = True }) nc 
                            : repeat (Launch (Right $ e2c { e2Restocker = False }) nc)
  man <- newManager defaultManagerSettings
  let mkReq :: (ExpCmd String,String) -> IO (Int,String)
      mkReq (cmd,addr) = do
        ireq <- parseRequest addr
        let req = ireq { requestBody = RequestBodyLBS $ encode cmd }
        resp <- httpLbs req man
        case responseStatus $ resp of
          c | c == status200 -> case decode (responseBody resp) of
                                  Just n -> return (n,addr)
          c -> print c >> die "Oops"
  exps <- traverse mkReq (zip cmds (Map.elems addrs))
  putStrLn "All nodes started experiment." >> hFlush stdout
  threadDelay $ (getTime ec + 20) * 1000000
  let getRes :: (Int,String) -> IO (Either ExpResult Exp2Result)
      getRes (n,addr) = do
        ireq <- parseRequest addr
        let req = ireq { requestBody = RequestBodyLBS $ encode (Report n :: ExpCmd String) }
        resp <- httpLbs req man
        case responseStatus $ resp of
          c | c == status200 -> case decode (responseBody resp) of
                                  Just d -> return d
            | otherwise -> print c >> die "Oops"
  results <- case ec of
    Left _ -> Left . fold . map unLeft <$> traverse getRes exps
    Right _ -> Right . fold . map unRight <$> traverse getRes exps
  case (ec,results) of
    (Left ec,Left results) -> do
      putStrLn $ "* Individual latencies"
      mapM_ (\(s,l) -> putStrLn $ s ++ ": " ++ show l ++ " s") (Map.assocs (expAvgLatencies results))
      putStrLn $ "* Failures"
      print $ expUnfinished results
      putStrLn $ "* Total measures"
      putStrLn $ "Total requests: " ++ show (totalReqs results) ++ " req"
      putStrLn $ "True rate: " ++ show (trueReqRate ec results) ++ " req/s"
      putStrLn $ "Avg latency: " ++ show (combinedAvgLatency results) ++ " s"
    (Right ec, Right results) -> do
      putStrLn $ "Total sales: " ++ show (e2TotalSales results)

unLeft (Left a) = a
unRight (Right a) = a
