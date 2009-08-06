module FRP.Peakachu.Backend.Time (
  timeOf, zipTime
  ) where

import FRP.Peakachu.Internal (Event(..), argument, eventBoo)

import Data.Time.Clock (UTCTime, getCurrentTime)

zipTime :: Event a -> Event (UTCTime, a)
zipTime =
  eventBoo (argument srcHandler)
  where
    srcHandler handler val = do
      now <- getCurrentTime
      handler (now, val)

timeOf :: Event a -> Event UTCTime
timeOf = fmap fst . zipTime

