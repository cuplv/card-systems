module Lang.Carol 
  ( -- * Building Carol operations
    Carol
  , issue
  , query
  , queryT
  , consume
  , produce
  , assert
  , whenBelow

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

-- | When store val is below given value, perform the given operation.
whenBelow :: (CARD s, Ord s) => s -> Carol s a -> Carol s (Maybe a)
whenBelow t op = do
  s <- queryT
  if s < t
     then Just <$> op
     else return Nothing
