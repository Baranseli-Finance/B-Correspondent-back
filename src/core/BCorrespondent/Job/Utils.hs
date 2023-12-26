{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE OverloadedStrings #-}

module BCorrespondent.Job.Utils (withElapsedTime, forever) where

import Control.Monad.Time (currentTime)
import Katip
import BCorrespondent.ServerM 

withElapsedTime :: String -> KatipContextT ServerM () -> KatipContextT ServerM ()
withElapsedTime loc job = recordTime ": ---> starts at " >> job >> recordTime ": ---> ends at "
  where recordTime msg = currentTime >>= ($(logTM) InfoS . logStr . ((loc <> msg) <>) . show)

forever :: Applicative f => f a -> f a
forever f = f *> forever f
{-# NOINLINE forever #-}