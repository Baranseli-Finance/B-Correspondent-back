{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module BCorrespondent.Statement.Transaction () where

data Status = Registered | ForwardedToElekse | Confirmed | Declined