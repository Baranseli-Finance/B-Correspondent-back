{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module BCorrespondent.Api.WS (WSApi (..)) where

import Servant.API.Extended ((:>), (:-))
import Servant.API.Generic (Generic)
import Servant.API.WebSocket (WebSocketPending)

data WSApi route =
     WSApi 
     { _wsApiNotifyWithdrawRecord ::
        route
          :- "institution"
          :> "fiat"
          :> "withdraw"
          :> "history"
          :> "item"
          :> "update"
          :> WebSocketPending,
      _wsApiNotifyTransactionUpdate ::
        route
          :- "frontend"
          :> "user"
          :> "dashboard"
          :> "transaction"
          :> "update"
          :> WebSocketPending,
      _wsApiNotifyWalletUpdate ::
        route
          :- "frontend"
          :> "user"
          :> "dashboard"
          :> "wallet"
          :> "update"
          :> WebSocketPending,
      _wsApiNotifyBalancedBookTransactionAdd ::
        route
          :- "frontend"
          :> "user"
          :> "balanced-book"
          :> "transaction"
          :> "add"
          :> WebSocketPending,
      _wsApiNotifyBalancedBookWalletUpdate ::
        route
          :- "frontend"
          :> "user"
          :> "balanced-book"
          :> "wallet"
          :> "update"
          :> WebSocketPending,
      _wsApiNotifyNotification ::
        route
          :- "frontend"
          :> "user"
          :> "notification"
          :> WebSocketPending
     }
     deriving stock (Generic)
