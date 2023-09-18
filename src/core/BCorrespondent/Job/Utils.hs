module BCorrespondent.Job.Utils (withElapsedTime) where

import Data.Time.Clock (getCurrentTime)
import Katip

withElapsedTime :: (Severity -> LogStr -> IO ()) -> String -> IO () -> IO ()
withElapsedTime logger loc job = recordTime ": ---> starts at " >> job >> recordTime ": ---> ends at "
  where 
    recordTime msg = do
      tm <- getCurrentTime
      logger InfoS $ logStr $ loc <> msg <> show tm