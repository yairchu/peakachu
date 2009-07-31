module FRP.Peakachu.Backend.Time (
  timeOf, zipTime
  ) where

import FRP.Peakachu.Internal (Event(..), EventEval(..))

import Data.Time.Clock (UTCTime, getCurrentTime)

zipTime :: Event a -> Event (UTCTime, a)
zipTime event =
  Event $ do
    ev <- runEvent event
    startTime <- getCurrentTime
    let
      srcHandler handler val = do
        now <- getCurrentTime
        handler (now, val)
    return EventEval {
      addHandler = addHandler ev . srcHandler,
      initialValues = map ((,) startTime) (initialValues ev)
      }

timeOf :: Event a -> Event UTCTime
timeOf = fmap fst . zipTime

