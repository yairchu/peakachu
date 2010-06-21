-- | A Peakachu backend to get the time

module FRP.Peakachu.Backend.Time
    ( getTimeB
    ) where

import Data.Monoid (Monoid(..))
import FRP.Peakachu.Backend (Backend(..))
import FRP.Peakachu.Backend.Internal (Sink(..))

import Data.Time.Clock (UTCTime, getCurrentTime)

getTimeB :: Backend a (UTCTime, a)
getTimeB =
    Backend f
    where
        f handler =
            return mempty { sinkConsume = consume }
            where
                consume tag = do
                    now <- getCurrentTime
                    handler (now, tag)
