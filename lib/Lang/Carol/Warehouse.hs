{-# LANGUAGE LambdaCase #-}

module Lang.Carol.Warehouse
  ( -- * Using reservations and queries
    sellR
  , restockQR
    -- * Using queries only
  , sellQ
  , restockQQ
  ) where

import Lang.Carol



-- | Sell a unit, using a reservation for safety.
sellR :: Carol Counter Bool
sellR = do
  let e = ef $ Sub 1
  consume e >>= \case
    Just _ -> do issue e
                 return True
    Nothing -> return False

-- | Sell a unit, using a query for safety.
sellQ :: Carol Counter Bool
sellQ = do
  (Counter s) <- query (cr$ LEQ)
  if s >= 1
     then issue (ef$ Sub 1) >> return True
     else return False

-- | Restock, producing reservations.
restockQR :: Int -> Carol Counter Int
restockQR maxSpace = do
  n <- restockQQ maxSpace
  if n > 0
     then produce (ef$ Sub n) >> return n
     else return 0

-- | Restock without producing reservations (so that units can be
-- claimed using queries).
restockQQ :: Int -> Carol Counter Int
restockQQ maxSpace = do
  (Counter s) <- query (cr$ GEQ)
  let n = maxSpace - s
  if n > 0
     then issue (ef$ Add n) >> return n
     else return 0
