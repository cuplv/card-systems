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
sellR :: Carol Counter (Either String ())
sellR = do
  let e = ef $ Sub 1
  consume e
  issue e
  return (Right ())

-- | Sell a unit, using a query for safety.
sellQ :: Carol Counter (Either String ())
sellQ = do
  (Counter s) <- query (cr$ LEQ)
  if s >= 1
     then issue (ef$ Sub 1) >> return (Right ())
     else return (Left "No unit to sell.")

-- | Restock, producing reservations.
restockQR :: Int -> Carol Counter Int
restockQR maxSpace = do
  n <- restockQQ maxSpace
  if n > 0
     then produce (ef$ Sub n)
     else return ()
  return n

-- | Restock without producing reservations (so that units can be
-- claimed using queries).
restockQQ :: Int -> Carol Counter Int
restockQQ maxSpace = do
  (Counter s) <- query (cr$ GEQ)
  let n = maxSpace - s
  if n > 0
     then issue (ef$ Add n)
     else return ()
  return n
