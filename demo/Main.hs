{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
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

main :: IO ()
main = node


data ConfCLI = ConfCLI
  { confFile :: FilePath
  , nodeName :: String
  , ipfsPort :: Int}

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
      misc = (fullDesc
              <> progDesc "Run a bank account CARD node"
              <> header "discard-demo - a demo application for the Carol language")
  in info (parser <**> helper) misc

node :: IO ()
node = do
  conf <- confCLI
  net <- decodeFileEither (confFile conf) >>= \case
    Right net -> return net
    Left exc -> print exc >> die "Could not read network configuration file"
  let script i man = do
        liftIO $ putStr (i ++ " $ ") >> hFlush stdout
        words <$> getLine >>= \case
          ["dp",a] -> runCarolM man (const $ return ()) (deposit (read a))
          ["wd",a] -> runCarolR man  (withdraw (read a)) >>= \case
                        Left e -> putStrLn e
                        _ -> return ()
          ["check"] -> print =<< runCarolR man (current)
          ["check","exact"] -> print =<< runCarolR man (currentS)
          _ -> putStrLn "Try again."
        script i man
  runNode (nodeName conf) (ipfsPort conf) net (Counter 0) 100000 1 script
  return ()