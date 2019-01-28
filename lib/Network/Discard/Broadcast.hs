{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Discard.Broadcast 
  ( BMsg (..)
  , Transport (..)
  , Dest (..)
  , Src (..)
  , Carries (..)
  , HttpT (..)
  , msgGetter
  , NetConf (..)
  , others
  , self
  , defaultPort
  , broadcast
  , helloAll

  ) where

import Control.Exception (catch)
import Control.Concurrent.STM
import Data.Aeson
import qualified Network.HTTP.Client as Client
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Network.HTTP.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

-- | 'BCast' constructs a broadcast for whatever data is being
-- carried, and 'Hello' constructs a simple "I'm new here"
-- announcement, which should be responded to with a 'BCast' update.
data BMsg s = BCast s | Hello deriving (Generic)

instance (ToJSON s) => ToJSON (BMsg s) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON s) => FromJSON (BMsg s)

class Transport t where
  data Dest t
  data Src t
  type Res t :: * -> *
  -- | Send a 'Hello' message
  hello :: Dest t -> Res t ()

class Carries t s where
  send :: Dest t -> BMsg s -> Res t ()
  listen :: Src t -> (BMsg s -> Res t Bool) -> Res t ()

---

instance Transport (TChan (BMsg s)) where
  data Dest (TChan (BMsg s)) = OutChan (TChan (BMsg s))
  data Src (TChan (BMsg s)) = InChan (TChan (BMsg s))
  type Res (TChan (BMsg s)) = STM
  hello (OutChan chan) = writeTChan chan Hello

instance Carries (TChan (BMsg s)) s where
  send (OutChan chan) msg = writeTChan chan msg
  listen (InChan chan) handle = do
    msg <- readTChan chan
    cont <- handle msg
    if cont
       then listen (InChan chan) handle
       else return ()

data HttpT

instance Transport HttpT where
  data Dest HttpT = HttpDest Client.Manager Client.Request
  data Src HttpT = HttpSrc Port
  type Res HttpT = IO
  hello (HttpDest man dest) = do
    let req = dest { Client.method = "GET"
                   , Client.requestBody = Client.RequestBodyLBS $ "HELLO" }
    catch (Client.httpLbs req man >> return ()) (\(Client.HttpExceptionRequest _ _) -> return ())
    -- putStr "SEND: " >> print (encode msg)
    return ()

msgGetter :: (ToJSON (BMsg s), FromJSON (BMsg s)) => (BMsg s -> IO Bool) -> Application
msgGetter handle request respond = do
  body <- strictRequestBody request
  -- Check for HELLO, otherwise parse as JSON
  case body of
    "HELLO" -> handle Hello
    _ -> case decode body of
           Just msg -> handle msg
  respond $ responseLBS status200 [] "OK"

instance (ToJSON (BMsg s), FromJSON (BMsg s)) => Carries HttpT s where
  send (HttpDest man dest) msg = do
    let req = dest { Client.method = "POST"
                   , Client.requestBody = Client.RequestBodyLBS $ encode msg }
    catch (Client.httpLbs req man >> return ()) (\(Client.HttpExceptionRequest _ _) -> return ())
    -- putStr "SEND: " >> print (encode msg)
    return ()
  listen (HttpSrc p) handle = do
    runSettings (setHost "!6" . setPort p $ defaultSettings) (msgGetter handle)

-- | A 'NetConf' associates a 'String' hostname and 'Int' port number
-- to a set of 'i'-named replica nodes.
data NetConf i = NetConf (Map i (String, Int)) deriving (Show,Eq,Ord)

others :: (Ord i) => i -> NetConf i -> [(i,(String,Int))]
others i (NetConf m) = filter (\(i',_) -> i /= i') (Map.toList m)

self i (NetConf m) = Map.lookup i m

instance (ToJSON i) => ToJSON (NetConf i) where
  toJSON (NetConf m) = 
    toJSON (map ent $ Map.assocs m)
    where ent (i,(h,p)) = object ["name" .= i
                                 ,"host" .= h
                                 ,"port" .= p]

instance (Ord i, FromJSON i) => FromJSON (NetConf i) where
  parseJSON = withArray "NodeList" $ \v -> NetConf <$> do
    foldM unpack Map.empty v
    where unpack m = withObject "Node" $ \v -> do
            name <- v .: "name"
            host <- v .:? "host" .!= "localhost"
            port <- v .:? "port" .!= defaultPort
            return (Map.insert name (host,port) m)

defaultPort = 23001 :: Int

broadcast :: (Monad (Res t), Transport t, Carries t s) => [Dest t] -> s -> Res t ()
broadcast others s = mapM_ (flip send (BCast s)) others

-- | Send a 'Hello' message to all destinations
helloAll :: (Monad (Res t), Transport t) => [Dest t] -> Res t ()
helloAll others = mapM_ hello others
