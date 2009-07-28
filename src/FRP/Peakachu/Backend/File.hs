module FRP.Peakachu.Backend.File (
  readFileE, writeFileE
  ) where

import FRP.Peakachu.Internal (Event, SideEffect(..), ejoin)

readFileE :: Event FilePath -> Event String
readFileE = ejoin . fmap readFile

writeFileE :: Event (FilePath, String) -> SideEffect
writeFileE = SideEffect . fmap (uncurry writeFile)

