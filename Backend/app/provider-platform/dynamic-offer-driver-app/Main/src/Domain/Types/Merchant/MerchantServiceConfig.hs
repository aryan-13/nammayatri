{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Merchant.MerchantServiceConfig where

import qualified Data.List as List
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Kernel.External.AadhaarVerification as AadhaarVerification
import Kernel.External.AadhaarVerification.Interface.Types
import qualified Kernel.External.Call as Call
import Kernel.External.Call.Interface.Types
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Interface.Types
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types
import Kernel.External.Payment.Interface as Payment
import Kernel.External.SMS as Sms
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Verification as Verification
import Kernel.External.Verification.Interface.Types
import Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Text.Show
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data ServiceName
  = MapsService Maps.MapsService
  | SmsService Sms.SmsService
  | WhatsappService Whatsapp.WhatsappService
  | VerificationService Verification.VerificationService
  | AadhaarVerificationService AadhaarVerification.AadhaarVerificationService
  | CallService Call.CallService
  | PaymentService Payment.PaymentService
  | RentalPaymentService Payment.PaymentService
  | IssueTicketService Ticket.IssueTicketService
  | NotificationService Notification.NotificationService
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''ServiceName)

instance Show ServiceName where
  show (MapsService s) = "Maps_" <> show s
  show (SmsService s) = "Sms_" <> show s
  show (WhatsappService s) = "Whatsapp_" <> show s
  show (VerificationService s) = "Verification_" <> show s
  show (AadhaarVerificationService s) = "AadhaarVerification_" <> show s
  show (CallService s) = "Call_" <> show s
  show (PaymentService s) = "Payment_" <> show s
  show (RentalPaymentService s) = "RentalPayment_" <> show s
  show (IssueTicketService s) = "Ticket_" <> show s
  show (NotificationService s) = "Notification_" <> show s

instance Read ServiceName where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (MapsService v1, r2)
            | r1 <- stripPrefix "Maps_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (SmsService v1, r2)
                 | r1 <- stripPrefix "Sms_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (WhatsappService v1, r2)
                 | r1 <- stripPrefix "Whatsapp_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (VerificationService v1, r2)
                 | r1 <- stripPrefix "Verification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (AadhaarVerificationService v1, r2)
                 | r1 <- stripPrefix "AadhaarVerification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (CallService v1, r2)
                 | r1 <- stripPrefix "Call_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (PaymentService v1, r2)
                 | r1 <- stripPrefix "Payment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RentalPaymentService v1, r2)
                 | r1 <- stripPrefix "RentalPayment_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (IssueTicketService v1, r2)
                 | r1 <- stripPrefix "Ticket_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (NotificationService v1, r2)
                 | r1 <- stripPrefix "Notification_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

data ServiceConfigD (s :: UsageSafety)
  = MapsServiceConfig !MapsServiceConfig
  | SmsServiceConfig !SmsServiceConfig
  | WhatsappServiceConfig !WhatsappServiceConfig
  | VerificationServiceConfig !VerificationServiceConfig
  | AadhaarVerificationServiceConfig !AadhaarVerificationServiceConfig
  | CallServiceConfig !CallServiceConfig
  | PaymentServiceConfig !PaymentServiceConfig
  | RentalPaymentServiceConfig !PaymentServiceConfig
  | IssueTicketServiceConfig !Ticket.IssueTicketServiceConfig
  | NotificationServiceConfig !NotificationServiceConfig
  deriving (Generic, Eq)

type ServiceConfig = ServiceConfigD 'Safe

instance FromJSON (ServiceConfigD 'Unsafe)

instance ToJSON (ServiceConfigD 'Unsafe)

data MerchantServiceConfigD (s :: UsageSafety) = MerchantServiceConfig
  { merchantId :: Id Merchant,
    serviceConfig :: ServiceConfigD s,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantServiceConfig = MerchantServiceConfigD 'Safe

instance FromJSON (MerchantServiceConfigD 'Unsafe)

instance ToJSON (MerchantServiceConfigD 'Unsafe)

getServiceName :: MerchantServiceConfig -> ServiceName
getServiceName osc = case osc.serviceConfig of
  MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig _ -> MapsService Maps.Google
    Maps.OSRMConfig _ -> MapsService Maps.OSRM
    Maps.MMIConfig _ -> MapsService Maps.MMI
    Maps.NextBillionConfig _ -> MapsService Maps.NextBillion
  SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig _ -> SmsService Sms.ExotelSms
    Sms.MyValueFirstConfig _ -> SmsService Sms.MyValueFirst
    Sms.GupShupConfig _ -> SmsService Sms.GupShup
  WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig _ -> WhatsappService Whatsapp.GupShup
  VerificationServiceConfig verifictaionCfg -> case verifictaionCfg of
    Verification.IdfyConfig _ -> VerificationService Verification.Idfy
    Verification.FaceVerificationConfig _ -> VerificationService Verification.InternalScripts
    Verification.GovtDataConfig -> VerificationService Verification.GovtData
  AadhaarVerificationServiceConfig aadhaarVerifictaionCfg -> case aadhaarVerifictaionCfg of
    AadhaarVerification.GridlineConfig _ -> AadhaarVerificationService AadhaarVerification.Gridline
  CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig _ -> CallService Call.Exotel
  PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> PaymentService Payment.Juspay
  RentalPaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig _ -> RentalPaymentService Payment.Juspay
  IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig _ -> IssueTicketService Ticket.Kapture
  NotificationServiceConfig notificationCfg -> case notificationCfg of
    Notification.FCMConfig _ -> NotificationService Notification.FCM
    Notification.PayTMConfig _ -> NotificationService Notification.PayTM
    Notification.GRPCConfig _ -> NotificationService Notification.GRPC

buildMerchantServiceConfig ::
  MonadTime m =>
  Id Merchant ->
  ServiceConfig ->
  Id DMOC.MerchantOperatingCity ->
  m MerchantServiceConfig
buildMerchantServiceConfig merchantId serviceConfig merchantOperatingCityId = do
  now <- getCurrentTime
  pure
    MerchantServiceConfig
      { merchantId,
        serviceConfig,
        merchantOperatingCityId = Just merchantOperatingCityId,
        updatedAt = now,
        createdAt = now
      }
