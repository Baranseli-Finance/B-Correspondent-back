{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

module BCorrespondent.Api.Http.Test (spec_api) where

import Data.Proxy
import BCorrespondent.Api.Auth (AuthApi)
import BCorrespondent.Api.Institution (InstitutionApi)
import BCorrespondent.Api.Frontend (FrontendApi)
import BCorrespondent.Transport.Model.Auth (Credentials, AuthToken, NewPassword, AuthCodeHash, ResendCode, AuthCode)
import BCorrespondent.Api.Handler.SendGrid.SendMail (SendGridSendMailRequest)
import BCorrespondent.Config (Email)
import BCorrespondent.Api.Foreign.SendGrid (SendGridApi)
import BCorrespondent.Transport.Model.Frontend (ProcuratoryRequest)
import BCorrespondent.Transport.Model.Invoice (InvoiceRegisterResponse)
import BCorrespondent.Transport.Model.Frontend 
       (Init, Sha, JWTStatus, DailyBalanceSheet, GapItem, 
        GapItemUnit, GapItemUnitStatus, GapItemTime, 
        InvoiceSince, GapItemAmount, HistoryTimeline, InitDashboard, 
        WalletType, Wallet, FetchGap, TimelineTransactionResponse,
        TimelineTransaction, Notifications, Notification, Issue,
        BalancedBook, BalancedBookInstitution, DayOfWeeksHourly, 
        AmountInDayOfWeek, DayOfWeeksHourlyTotalSum, BalancedBookWallet,
        GapItemWrapper)
import BCorrespondent.Transport.Model.Institution 
       (Withdraw, InitWithdrawal, Balance, WithdrawalHistoryItem, 
        WithdrawalHistory, WithdrawResult, WithdrawResultStatus,
        WithdrawalHistoryItem)

import Servant.API.Generic
import Servant.Swagger.Test
import Test.Hspec
import TH.Mk (mkArbitrary)

mkArbitrary ''AuthToken
mkArbitrary ''Credentials
mkArbitrary ''Email
mkArbitrary ''SendGridSendMailRequest
mkArbitrary ''NewPassword
mkArbitrary ''ProcuratoryRequest
mkArbitrary ''InvoiceRegisterResponse
mkArbitrary ''JWTStatus
mkArbitrary ''Sha
mkArbitrary ''GapItemTime
mkArbitrary ''GapItemAmount
mkArbitrary ''GapItemUnitStatus
mkArbitrary ''GapItemUnit
mkArbitrary ''GapItem
mkArbitrary ''GapItemWrapper
mkArbitrary ''WalletType
mkArbitrary ''Wallet
mkArbitrary ''FetchGap
mkArbitrary ''TimelineTransaction
mkArbitrary ''TimelineTransactionResponse
mkArbitrary ''DailyBalanceSheet
mkArbitrary ''InitDashboard
mkArbitrary ''InvoiceSince
mkArbitrary ''HistoryTimeline
mkArbitrary ''Init
mkArbitrary ''AuthCodeHash
mkArbitrary ''ResendCode
mkArbitrary ''AuthCode
mkArbitrary ''Balance
mkArbitrary ''Withdraw
mkArbitrary ''WithdrawResultStatus
mkArbitrary ''WithdrawalHistoryItem
mkArbitrary ''WithdrawalHistory
mkArbitrary ''WithdrawResult
mkArbitrary ''InitWithdrawal
mkArbitrary ''Notification
mkArbitrary ''Notifications
mkArbitrary ''Issue
mkArbitrary ''DayOfWeeksHourlyTotalSum
mkArbitrary ''AmountInDayOfWeek
mkArbitrary ''DayOfWeeksHourly
mkArbitrary ''BalancedBookWallet
mkArbitrary ''BalancedBookInstitution
mkArbitrary ''BalancedBook

spec_api :: Spec
spec_api =
  describe "Swagger spec for API" $ do
    context "ToJSON matches ToSchema (AuthApi)" $
      validateEveryToJSON (genericApi (Proxy @AuthApi))
    context "ToJSON matches ToSchema (SendGridApi)" $
      validateEveryToJSON (genericApi (Proxy @SendGridApi))
    context "ToJSON matches ToSchema (InvoiceApi)" $
      validateEveryToJSON (genericApi (Proxy @InstitutionApi))
    context "ToJSON matches ToSchema (FrontendApi)" $
      validateEveryToJSON (genericApi (Proxy @FrontendApi))