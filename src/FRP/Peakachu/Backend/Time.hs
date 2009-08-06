module FRP.Peakachu.Backend.Time (
  timeOf, zipTime
  ) where

import FRP.Peakachu.Internal (Event(..), inEvent, argument)

import Data.Time.Clock (UTCTime, getCurrentTime)

zipTime :: Event a -> Event (UTCTime, a)
zipTime =
  inEvent $ argument srcHandler
  where
    srcHandler handler val = do
      now <- getCurrentTime
      handler (now, val)

timeOf :: Event a -> Event UTCTime
timeOf = fmap fst . zipTime

