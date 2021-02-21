module Lang.Carol 
  ( -- * Building Carol operations
    Carol
  , issue
  , query
  , queryT
  , consume
  , produce
  , assert

    -- * Running Carol operations
  , CCarrier
  , carol
  , carolAsync
  , carolAsync'

  , module Data.CARD

  ) where

import Lang.Carol.Internal
import Data.CARD

assert :: Bool -- ^ Assert condition
       -> String -- ^ Failure message
       -> Carol s (Either String a) -- ^ Remaining term
       -> Carol s (Either String a)
assert b s op = if b
                   then op
                   else return (Left s)
