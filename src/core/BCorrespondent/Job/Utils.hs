{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}

module BCorrespondent.Job.Utils (withElapsedTime) where

import Control.Monad.Time (currentTime)
import Katip
import BCorrespondent.ServerM 

withElapsedTime :: String -> KatipContextT ServerM () -> KatipContextT ServerM ()
withElapsedTime loc job = recordTime ": ---> starts at " >> job >> recordTime ": ---> ends at "
  where 
    recordTime msg = do
      tm <- currentTime
      $(logTM) InfoS $ logStr $ loc <> msg <> show tm