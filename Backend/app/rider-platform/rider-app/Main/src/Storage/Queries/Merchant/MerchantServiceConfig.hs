{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.MerchantServiceConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Data.Aeson as A
import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.Merchant.MerchantServiceConfig as Domain
import Kernel.Beam.Functions
import qualified Kernel.External.AadhaarVerification.Interface as AadhaarVerification
import qualified Kernel.External.Call as Call
import qualified Kernel.External.Maps.Interface.Types as Maps
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types as Notification
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.SMS.Interface as Sms
import Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Whatsapp.Interface as Whatsapp
import Kernel.Prelude as P
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantServiceConfig as BeamMSC
import Tools.Error

-- create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => MerchantServiceConfig -> m ()
-- create = createWithKV

-- findAllMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m [MerchantServiceConfig]
-- findAllMerchantOpCityId (Id merchantOperatingCityId) = findAllWithKV [Se.Is BeamMSC.merchantOperatingCityId $ Se.Eq $ Just merchantOperatingCityId]

findByMerchantIdAndService :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService (Id merchantId) serviceName = findOneWithKV [Se.And [Se.Is BeamMSC.merchantId $ Se.Eq merchantId, Se.Is BeamMSC.serviceName $ Se.Eq serviceName]]

upsertMerchantServiceConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantServiceConfig -> m ()
upsertMerchantServiceConfig merchantServiceConfig = do
  now <- getCurrentTime
  let (_serviceName, configJSON) = BeamMSC.getServiceNameConfigJSON merchantServiceConfig.serviceConfig
  res <- findByMerchantIdAndService merchantServiceConfig.merchantId _serviceName
  if isJust res
    then
      updateWithKV
        [Se.Set BeamMSC.configJSON configJSON, Se.Set BeamMSC.updatedAt now]
        [Se.Is BeamMSC.merchantId $ Se.Eq $ getId merchantServiceConfig.merchantId]
    else createWithKV merchantServiceConfig

instance FromTType' BeamMSC.MerchantServiceConfig MerchantServiceConfig where
  fromTType' BeamMSC.MerchantServiceConfigT {..} = do
    serviceConfig <- getServiceConfigFromDomain serviceName configJSON
    pure $
      Just
        MerchantServiceConfig
          { merchantId = Id merchantId,
            serviceConfig = serviceConfig,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

valueToMaybe :: FromJSON a => A.Value -> Maybe a
valueToMaybe value = case A.fromJSON value of
  A.Success a -> Just a
  _ -> Nothing

getServiceConfigFromDomain :: (MonadFlow m) => Domain.ServiceName -> A.Value -> m Domain.ServiceConfig
getServiceConfigFromDomain serviceName configJSON = do
  maybe (throwError $ InternalError "Unable to decode MerchantServiceConfigT.configJSON") return $ case serviceName of
    Domain.MapsService Maps.Google -> Domain.MapsServiceConfig . Maps.GoogleConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.OSRM -> Domain.MapsServiceConfig . Maps.OSRMConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.MMI -> Domain.MapsServiceConfig . Maps.MMIConfig <$> valueToMaybe configJSON
    Domain.MapsService Maps.NextBillion -> Domain.MapsServiceConfig . Maps.NextBillionConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.ExotelSms -> Domain.SmsServiceConfig . Sms.ExotelSmsConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.MyValueFirst -> Domain.SmsServiceConfig . Sms.MyValueFirstConfig <$> valueToMaybe configJSON
    Domain.SmsService Sms.GupShup -> Domain.SmsServiceConfig . Sms.GupShupConfig <$> valueToMaybe configJSON
    Domain.WhatsappService Whatsapp.GupShup -> Domain.WhatsappServiceConfig . Whatsapp.GupShupConfig <$> valueToMaybe configJSON
    Domain.CallService Call.Exotel -> Domain.CallServiceConfig . Call.ExotelConfig <$> valueToMaybe configJSON
    Domain.CallService Call.Knowlarity -> Nothing
    Domain.AadhaarVerificationService AadhaarVerification.Gridline -> Domain.AadhaarVerificationServiceConfig . AadhaarVerification.GridlineConfig <$> valueToMaybe configJSON
    Domain.NotificationService Notification.FCM -> Domain.NotificationServiceConfig . Notification.FCMConfig <$> valueToMaybe configJSON
    Domain.NotificationService Notification.PayTM -> Domain.NotificationServiceConfig . Notification.PayTMConfig <$> valueToMaybe configJSON
    Domain.NotificationService Notification.GRPC -> Domain.NotificationServiceConfig . Notification.GRPCConfig <$> valueToMaybe configJSON
    Domain.PaymentService Payment.Juspay -> Domain.PaymentServiceConfig . Payment.JuspayConfig <$> valueToMaybe configJSON
    Domain.IssueTicketService Ticket.Kapture -> Domain.IssueTicketServiceConfig . Ticket.KaptureConfig <$> valueToMaybe configJSON

instance ToTType' BeamMSC.MerchantServiceConfig MerchantServiceConfig where
  toTType' MerchantServiceConfig {..} =
    BeamMSC.MerchantServiceConfigT
      { BeamMSC.merchantId = getId merchantId,
        BeamMSC.serviceName = fst $ getServiceNameConfigJson serviceConfig,
        BeamMSC.configJSON = snd $ getServiceNameConfigJson serviceConfig,
        BeamMSC.updatedAt = updatedAt,
        BeamMSC.createdAt = createdAt
      }

getServiceNameConfigJson :: Domain.ServiceConfig -> (Domain.ServiceName, A.Value)
getServiceNameConfigJson = \case
  Domain.MapsServiceConfig mapsCfg -> case mapsCfg of
    Maps.GoogleConfig cfg -> (Domain.MapsService Maps.Google, toJSON cfg)
    Maps.OSRMConfig cfg -> (Domain.MapsService Maps.OSRM, toJSON cfg)
    Maps.MMIConfig cfg -> (Domain.MapsService Maps.MMI, toJSON cfg)
    Maps.NextBillionConfig cfg -> (Domain.MapsService Maps.NextBillion, toJSON cfg)
  Domain.SmsServiceConfig smsCfg -> case smsCfg of
    Sms.ExotelSmsConfig cfg -> (Domain.SmsService Sms.ExotelSms, toJSON cfg)
    Sms.MyValueFirstConfig cfg -> (Domain.SmsService Sms.MyValueFirst, toJSON cfg)
    Sms.GupShupConfig cfg -> (Domain.SmsService Sms.GupShup, toJSON cfg)
  Domain.WhatsappServiceConfig whatsappCfg -> case whatsappCfg of
    Whatsapp.GupShupConfig cfg -> (Domain.WhatsappService Whatsapp.GupShup, toJSON cfg)
  Domain.CallServiceConfig callCfg -> case callCfg of
    Call.ExotelConfig cfg -> (Domain.CallService Call.Exotel, toJSON cfg)
  Domain.NotificationServiceConfig notificationCfg -> case notificationCfg of
    Notification.FCMConfig cfg -> (Domain.NotificationService Notification.FCM, toJSON cfg)
    Notification.PayTMConfig cfg -> (Domain.NotificationService Notification.PayTM, toJSON cfg)
    Notification.GRPCConfig cfg -> (Domain.NotificationService Notification.GRPC, toJSON cfg)
  Domain.AadhaarVerificationServiceConfig aadhaarVerificationCfg -> case aadhaarVerificationCfg of
    AadhaarVerification.GridlineConfig cfg -> (Domain.AadhaarVerificationService AadhaarVerification.Gridline, toJSON cfg)
  Domain.PaymentServiceConfig paymentCfg -> case paymentCfg of
    Payment.JuspayConfig cfg -> (Domain.PaymentService Payment.Juspay, toJSON cfg)
  Domain.IssueTicketServiceConfig ticketCfg -> case ticketCfg of
    Ticket.KaptureConfig cfg -> (Domain.IssueTicketService Ticket.Kapture, toJSON cfg)
