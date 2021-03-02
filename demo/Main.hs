{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import System.Directory
import System.Exit
import Data.List (genericLength)
import Control.Monad
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Concurrent (forkIO,ThreadId,threadDelay)
import Control.Concurrent.STM hiding (check)
import Network.HTTP.Client
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Yaml
import Options.Applicative
import System.Random
import Data.Time.Clock

import Lang.Carol
import Lang.Carol.Bank
import Network.Discard
import Data.EventGraph
import Data.CARD.Store
import Data.EventGraph.Ipfs (IpfsEG)

import Interface

main :: IO ()
main = node

data ConfCLI = ConfCLI
  { confFile :: FilePath
  , nodeName :: String
  , ipfsPort :: Int
  , pFile :: Maybe FilePath
  , isOneshot :: Bool }

localAddr :: ConfCLI -> String
localAddr c = "http://localhost:" <> show (ipfsPort c)

optionm p os = (Just <$> option p os) <|> pure Nothing

confCLI :: IO ConfCLI
confCLI = execParser $ 
  let parser = ConfCLI
        <$> strOption (short 'c' 
                       <> metavar "FILE" 
                       <> help "Network configuration file")
        <*> strOption (short 'i' 
                       <> metavar "NAME" 
                       <> help "Node name")
        <*> option auto (long "ipfs-port" <> value 5001)
        <*> optionm str (short 'f' <> metavar "FILE" <> help "Persistent storage file")
        <*> switch (long "oneshot" <> help "Deposit 10 and exit, instead of interactive session")
      misc = (fullDesc
              <> progDesc "Run a bank account CARD node"
              <> header "discard-demo - a demo application for the Carol language")
  in info (parser <**> helper) misc

tryPFile :: FilePath -> IO (Maybe (Counter, Hist (Edge (IpfsEG String)) String Counter))
tryPFile fp = doesFileExist fp >>= \case
  True -> decodeFileEither fp >>= \case
    Right state -> return (Just state)
    Left e -> do print e
                 die $ "Save file \"" <> fp <> "\" exists but is unreadable."
  False -> return Nothing

emptyInit = (Counter 0, Data.EventGraph.empty)


node :: IO ()
node = do
  conf <- confCLI
  net <- decodeFileEither (confFile conf) >>= \case
    Right net -> return net
    Left exc -> print exc >> die "Could not read network configuration file"

  let runNode' settings script = case pFile conf of
        Just sfile -> 
          runNodeFile (nodeName conf) (localAddr conf) net sfile settings script
        Nothing -> do
          runNode (nodeName conf) True (localAddr conf) net settings script

  if isOneshot conf

     then do let settings0 = defaultDManagerSettings
             (settings, await) <- awaitNetwork settings0 (Just 1000000)
             let script i man = do
                   await >>= \case
                     False -> putStrLn "Network timeout. Continuing in offline mode..."
                     _ -> return ()
                   Counter s0 <- carol man queryT
                   putStrLn $ "Starting balance: $" <> show s0 <> "."
                   d <- carol man $ deposit 10
                   case d of
                     Right n -> putStrLn $ "Deposited $" <> show n <> "."
                     Left e -> putStrLn e
                   Counter s1 <- carol man $ queryT
                   putStrLn $ "Ending balance: $" <> show s1 <> "."
             runNode' settings script

     else do (eventChan, onUpdate, onMessage) <- mkUpdateChan
             let onUpdate' (v,s) = onUpdate (v,getRess s)
                 script i man = do 
                   initStore <- carol man $ queryT
                   runUi (nodeName conf) initStore man eventChan
                 settings = defaultDManagerSettings { onUpdate = onUpdate'
                                                    , onGetBroadcast = onMessage }
             runNode' settings script

