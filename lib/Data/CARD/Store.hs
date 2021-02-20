{-# LANGUAGE FlexibleContexts #-}

module Data.CARD.Store
  ( Hist
  , evalHist
  , Store
  , locks
  , hist
  , ress
  , histAppend
  -- * Convenient re-exports
  , module Data.CARD
  , module Data.CARD.Locks
  ) where

import Data.Map (Map,lookup)
import Control.Lens

import Data.CvRDT
import Data.CARD
import Data.CARD.Locks
import Data.CARD.Res

-- | A 'Hist' is a sequence of CARD effects each paired with an 'i'
-- identifier naming the replica responsible for them.  The
-- identifiers, combined with each effect's history, make each element
-- of the sequence unique.
type Hist c i s = c (i, Effect s)

-- | Evaluate a 'Hist' according to the evaluation semantics defined
-- for 's' 'Effect's.  A "summaries" 'Map' from history prefixes to
-- store values can be provided as a shortcut.
evalHist :: (CARD s, CvChain r c (i, Effect s) m) 
         => r -- ^ Resolver
         -> s -- ^ Initial store value
         -> Hist c i s -- ^ History to evaluate
         -> Map (Hist c i s) s -- ^ Summaries
         -> m s
evalHist r s0 c0 summs = 
  foldlC 
    r 
    (\s -> runEffect s . snd)
    (flip Data.Map.lookup summs)
    s0
    c0

-- | A 'Store' for a 'CARD' 's' is a 'CvRDT' pair consisting of a
-- distributed lock state (the 'Locks'), a reservation store (the
-- 'Res') and an event history (the 'Hist').
type Store c i s = ((Locks i s, Hist c i s), Res i s)

-- | Lens to the lock state
locks :: Lens' (Store c i s) (Locks i s)
locks = _1 . _1

-- | Lens to the history structure
hist :: Lens' (Store c i s) (Hist c i s)
hist = _1 . _2

-- | Lens to the reservation store
ress :: Lens' (Store c i s) (Res i s)
ress = _2

-- | Emit a store effect, tagged with a replica ID.
histAppend :: (Ord i, CARD s, CvChain r c (i, Effect s) m) 
           => i 
           -> Effect s 
           -> CvRepCmd r (Store c i s) k m ()
histAppend i e = do 
  r <- use resolver
  emitOn' hist $ append r (i,e)
  return ()
