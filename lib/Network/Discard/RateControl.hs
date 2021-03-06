{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Discard.RateControl
  ( RCIndex
  , newRCIndex
  , getRCBlocker
  , reportFailure
  , reportSuccess
  , getRetry
  , enqueGrant
  , getGrant
  , awaitTimeouts 

  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Conc (forkIO,registerDelay,readTVar,TVar,threadDelay,ThreadId,killThread)
import Data.Time.Clock
import Control.Monad.Trans

import Data.CARD

lstm :: MonadIO m => STM a -> m a
lstm = liftIO . atomically

type Queue a = [a]

putQueue :: a -> Queue a -> Queue a
putQueue a as = as ++ [a]

takeQueue :: Queue a -> Maybe (a, Queue a)
takeQueue = \case
  (a:as) -> Just (a,as)
  [] -> Nothing

data RetryControl = Open | Control (Maybe (TVar Bool))

data RateControl g j = RateControl
  { rcIndex :: Int
  , rcReactOn :: Bool
  , rcGrantGate :: Maybe (TVar Bool)
  , rcRetryGate :: RetryControl
  , rcGrantQueue :: Queue g
  , rcRetryQueue :: TQueue j }

data RCIndex i s j = RCIndex
  { rciSettings :: RCSettings
  , rciBlocker :: Conref s
  , rciControl :: RateControl (i, Conref s) j }

-- | Create a new index
newRCIndex :: (CARD s) => NominalDiffTime -> IO (RCIndex i s j)
newRCIndex t = do
  rc <- RateControl 1 True Nothing Open [] <$> newTQueueIO
  return $ RCIndex (RCSettings t) crT rc

getRCBlocker :: (CARD s) => RCIndex i s j -> Conref s
getRCBlocker (RCIndex _ cb _) = cb

-- | Report a failure.  The 'Conref s' should be the smallest relevant
-- bit of the blocking lock.  The 'j' is the job that will be retried
-- later.
reportFailure :: (CARD s) => Conref s -> j -> RCIndex i s j -> IO (RCIndex i s j)
reportFailure c j (RCIndex st cb rc) = RCIndex st (cb |&| c) <$> reportFailure' st j rc


-- | Report a successful effect emission.
reportSuccess :: (CARD s) => Effect s -> RCIndex i s j -> IO (RCIndex i s j)
reportSuccess e (RCIndex st cb rc) = 
  if checkBlock cb e
     then do putStrLn ""
             putStrLn "(Success, reducing index by 1)"
             putStrLn ""
             return $ RCIndex st cb (reportSuccess' rc)
     else return $ RCIndex st cb rc

-- | Get a job to retry, if one is ready
getRetry :: RCIndex i s j -> IO (Maybe (j,RCIndex i s j))
getRetry (RCIndex st cb rc) = getRetry' st rc >>= \case
  Just (j,rc') -> return (Just (j, RCIndex st cb rc'))
  Nothing -> return Nothing

enqueGrant :: (Eq i, CARD s) => (i,Conref s) -> RCIndex i s j -> IO (RCIndex i s j)
enqueGrant g (RCIndex st cb rc) = do
  RCIndex st cb <$> enqueGrant' st g rc

-- | Get a grant to offer, if one is ready
getGrant :: RCIndex i s j -> IO (Maybe ((i, Conref s), RCIndex i s j))
getGrant (RCIndex st cb rc) = getGrant' st rc >>= \case
  Just (j,rc') -> return (Just (j, RCIndex st cb rc'))
  Nothing -> return Nothing

-- | Wait for one of the gates to open and return the changed index
awaitTimeouts :: RCIndex i s j -> STM (RCIndex i s j)
awaitTimeouts (RCIndex st cb rc) = do
  let grantOpen = RCIndex st cb $ rc { rcGrantGate = Nothing }
      gRead = case rcGrantGate rc of
                Just tv -> readTVar tv
                Nothing -> return False
      retryOpen = case rcRetryGate rc of
                    Control _ -> RCIndex st cb $ rc { rcRetryGate = Control Nothing }
                    _ -> RCIndex st cb rc
      rRead = case rcRetryGate rc of
                Control (Just tv) -> readTVar tv
                _ -> return False
      grantTrig = const grantOpen <$> (check =<< gRead)
      retryTrig = const retryOpen <$> (check =<< rRead)
  retryTrig <|> grantTrig

oneMil :: (Num a) => a
oneMil = 1000000

data RCSettings = RCSettings
  { baseTimeout :: NominalDiffTime }

initTimer :: NominalDiffTime -- ^ Base timeout unit
          -> Int -- ^ Congestion index
          -> Double -- ^ Randomized offset
          -> IO (TVar Bool)
initTimer base cong rmult = 
  let micros = floor 
               . (* oneMil)
               . min (toRational maxTimeout)
               . (* toRational rmult)
               . (* (toRational . fromIntegral $ cong))
               $ toRational base
  in registerDelay micros

setRetryTimer :: RCSettings -> RateControl g j -> IO (RateControl g j)
setRetryTimer sets rc = case rcRetryGate rc of
  Control Nothing -> do
    (rmult,_) <- randomR (0.5, 2.0) <$> newStdGen
    tm <- initTimer (baseTimeout sets / 2) (rcIndex rc) rmult
    return (rc { rcRetryGate = Control (Just tm) })
  _ -> return rc

setGrantTimer :: RCSettings -> RateControl g j -> IO (RateControl g j)
setGrantTimer sets rc = case rcGrantGate rc of
  Just _ -> return rc
  Nothing -> do
    (rmult,_) <- randomR (0.5, 2.0) <$> newStdGen
    tm <- initTimer (baseTimeout sets) (rcIndex rc) rmult
    return (rc { rcGrantGate = Just tm })

maxTimeout :: NominalDiffTime
maxTimeout = 4

reportFailure' :: RCSettings -> j -> RateControl g j -> IO (RateControl g j)
reportFailure' sets ji rc = do
  let rc' = case rcRetryGate rc of
        Open -> rc { rcRetryGate = Control Nothing } 
        _ -> rc
  if rcReactOn rc'
     then do lstm (writeTQueue (rcRetryQueue rc') ji)
             putStrLn ""
             putStrLn $ "********************"
             putStrLn $ "| Emit failure."
             putStrLn $ "| RC index up to: " ++ show (2 * rcIndex rc')
             putStrLn $ "| Timout up to: " ++ show (min maxTimeout (baseTimeout sets * fromIntegral (2 * rcIndex rc')))
             putStrLn $ "********************"
             putStrLn ""
             setRetryTimer sets (rc' { rcIndex = 2 * rcIndex rc' } { rcReactOn = False })
     else lstm (writeTQueue (rcRetryQueue rc') ji) >> return rc'


enqueGrant' :: (Eq g) => RCSettings -> g -> RateControl g j -> IO (RateControl g j)
enqueGrant' sets gi rc = 
  if not $ gi `elem` rcGrantQueue rc
     then setGrantTimer sets rc 
          >> return (rc { rcGrantQueue = putQueue gi (rcGrantQueue rc)})
     else return rc

reportSuccess' :: RateControl g j -> RateControl g j
reportSuccess' rc = rc { rcIndex = max 1 (rcIndex rc - 1) } { rcRetryGate = Open }

getRetry' :: RCSettings -> RateControl g j -> IO (Maybe (j, RateControl g j))
getRetry' sets rc = case rcRetryGate rc of
  Control (Just _) -> return Nothing
  _ -> lstm (tryReadTQueue (rcRetryQueue rc)) >>= \case
    Just j -> do rc' <- setRetryTimer sets rc
                 return (Just (j,rc' { rcReactOn = True }))
    Nothing -> return Nothing

getGrant' :: RCSettings -> RateControl g j -> IO (Maybe (g, RateControl g j))
getGrant' sets rc = case rcGrantGate rc of
  Just _ -> return Nothing
  Nothing -> case takeQueue (rcGrantQueue rc) of
    Just (g,q') -> do rc' <- setGrantTimer sets rc
                      return (Just (g,rc' { rcGrantQueue = q' }))
    Nothing -> return Nothing
