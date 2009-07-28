module FRP.Peakachu.Backend.File (
  readFileE, writeFileE
  ) where

import FRP.Peakachu.Internal (Event, ejoin)

readFileE :: Event FilePath -> Event String
readFileE = ejoin . fmap readFile

writeFileE :: Event (FilePath, String) -> Event ()
writeFileE = ejoin . fmap (uncurry writeFile)

