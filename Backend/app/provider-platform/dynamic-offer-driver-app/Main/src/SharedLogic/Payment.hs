{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Payment where

import Data.Time (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import Domain.Types.DriverFee
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.MerchantMessage as MessageKey
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan as DP
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Payment.Interface.Types
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import SharedLogic.DriverFee (roundToHalf)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.Invoice as QIN
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Payment as TPayment
import qualified Tools.SMS as Sms
import Tools.Whatsapp as Whatsapp

data MandateOrder = MandateOrder
  { maxAmount :: HighPrecMoney,
    _type :: MandateType,
    frequency :: MandateFrequency,
    mandateStartDate :: Text,
    mandateEndDate :: Text
  }

createOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DMSC.ServiceName ->
  ([DriverFee], [DriverFee]) ->
  Maybe MandateOrder ->
  INV.InvoicePaymentMode ->
  Maybe (Id INV.Invoice, Text) ->
  Maybe DeepLinkData ->
  m (CreateOrderResp, Id DOrder.PaymentOrder)
createOrder (driverId, merchantId, opCity) serviceName (driverFees, driverFeesToAddOnExpiry) mbMandateOrder invoicePaymentMode existingInvoice mbDeepLinkData = do
  mapM_ (\driverFee -> when (driverFee.status `elem` [CLEARED, EXEMPTED, COLLECTED_CASH]) $ throwError (DriverFeeAlreadySettled driverFee.id.getId)) driverFees
  mapM_ (\driverFee -> when (driverFee.status `elem` [INACTIVE, ONGOING]) $ throwError (DriverFeeNotInUse driverFee.id.getId)) driverFees
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound $ getId driverId)
  unless (driver.id == driverId) $ throwError NotAnExecutor
  driverPhone <- driver.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  genInvoiceId <- generateGUID
  genShortInvoiceId <- generateShortId
  now <- getCurrentTime
  let driverEmail = fromMaybe "test@juspay.in" driver.email
      (invoiceId, invoiceShortId) = fromMaybe (genInvoiceId, genShortInvoiceId.getShortId) existingInvoice
      amount = sum $ (\pendingFees -> roundToHalf (fromIntegral pendingFees.govtCharges + pendingFees.platformFee.fee + pendingFees.platformFee.cgst + pendingFees.platformFee.sgst)) <$> driverFees
      invoices = mkInvoiceAgainstDriverFee invoiceId.getId invoiceShortId now (mbMandateOrder <&> (.maxAmount)) invoicePaymentMode <$> driverFees
  when (amount <= 0) $ throwError (InternalError "Invalid Amount :- should be greater than 0")
  unless (isJust existingInvoice) $ QIN.createMany invoices
  let createOrderReq =
        CreateOrderReq
          { orderId = invoiceId.getId,
            orderShortId = invoiceShortId,
            amount = amount,
            customerId = driver.id.getId,
            customerEmail = driverEmail,
            customerPhone = driverPhone,
            customerFirstName = Just driver.firstName,
            customerLastName = driver.lastName,
            createMandate = mbMandateOrder <&> (._type),
            mandateMaxAmount = mbMandateOrder <&> (.maxAmount),
            mandateFrequency = mbMandateOrder <&> (.frequency),
            mandateEndDate = mbMandateOrder <&> (.mandateEndDate),
            mandateStartDate = mbMandateOrder <&> (.mandateStartDate),
            optionsGetUpiDeepLinks = mbDeepLinkData >>= (.sendDeepLink),
            metadataExpiryInMins = mbDeepLinkData >>= (.expiryTimeInMinutes)
          }
  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person driver.id
      createOrderCall = TPayment.createOrder merchantId opCity serviceName -- api call
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> return (createOrderRes, cast invoiceId)
    Nothing -> do
      QIN.updateInvoiceStatusByInvoiceId INV.EXPIRED invoiceId
      createOrder (driverId, merchantId, opCity) serviceName (driverFees <> driverFeesToAddOnExpiry, []) mbMandateOrder invoicePaymentMode Nothing mbDeepLinkData -- call same function with no existing order

mkInvoiceAgainstDriverFee :: Text -> Text -> UTCTime -> Maybe HighPrecMoney -> INV.InvoicePaymentMode -> DriverFee -> INV.Invoice
mkInvoiceAgainstDriverFee id shortId now maxMandateAmount paymentMode driverFee =
  INV.Invoice
    { id = Id id,
      invoiceShortId = shortId,
      driverFeeId = driverFee.id,
      invoiceStatus = INV.ACTIVE_INVOICE,
      driverId = driverFee.driverId,
      maxMandateAmount,
      paymentMode,
      bankErrorCode = Nothing,
      bankErrorMessage = Nothing,
      bankErrorUpdatedAt = Nothing,
      lastStatusCheckedAt = Nothing,
      serviceName = driverFee.serviceName,
      merchantOperatingCityId = driverFee.merchantOperatingCityId,
      updatedAt = now,
      createdAt = now
    }

offerListCache :: (MonadFlow m, ServiceFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DP.ServiceNames -> Payment.OfferListReq -> m Payment.OfferListResp
offerListCache merchantId merchantOpCityId serviceName req = do
  transporterConfig <- SCT.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  if transporterConfig.useOfferListCache
    then do
      key <- makeOfferListCacheKey transporterConfig.cacheOfferListByDriverId req
      Hedis.get key >>= \case
        Just a -> return a
        Nothing -> cacheOfferListResponse transporterConfig.cacheOfferListByDriverId req /=<< TPayment.offerList merchantId merchantOpCityId subscriptionConfig.paymentServiceName req
    else TPayment.offerList merchantId merchantOpCityId subscriptionConfig.paymentServiceName req

cacheOfferListResponse :: (MonadFlow m, CacheFlow m r) => Bool -> Payment.OfferListReq -> Payment.OfferListResp -> m ()
cacheOfferListResponse includeDriverId req resp = do
  key <- makeOfferListCacheKey includeDriverId req
  Hedis.setExp key resp 86400

makeOfferListCacheKey :: (MonadFlow m, CacheFlow m r) => Bool -> Payment.OfferListReq -> m Text
makeOfferListCacheKey includeDriverId req = do
  case (req.customer, includeDriverId) of
    (Just customer, True) -> do
      return $
        "OfferList:CId" <> customer.customerId <> ":PId-" <> req.planId <> ":PM-" <> req.paymentMode <> ":n-"
          <> show req.numOfRides
          <> ":dt-"
          <> show (utctDay req.dutyDate)
          <> ":ft-"
          <> show (utctDay req.registrationDate)
          <> ":Listing-"
          <> maybe "N/A" parseValidityForCaching req.offerListingMetric
    _ ->
      return $
        "OfferList:PId-" <> req.planId <> ":PM-" <> req.paymentMode <> ":n-" <> show req.numOfRides <> ":dt-"
          <> show (utctDay req.dutyDate)
          <> ":ft-"
          <> show (utctDay req.registrationDate)
          <> ":Listing-"
          <> maybe "N/A" parseValidityForCaching req.offerListingMetric
  where
    parseValidityForCaching offerListingMetric' =
      case offerListingMetric' of
        LIST_BASED_ON_DATE listingDates -> show $ UTCTime (utctDay listingDates) (secondsToDiffTime 0)
        _ -> show offerListingMetric'

sendLinkTroughChannelProvided ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasField "smsCfg" r SmsConfig) =>
  Maybe Payment.PaymentLinks ->
  Id DP.Person ->
  Maybe HighPrecMoney ->
  Maybe MediaChannel ->
  Bool ->
  MessageKey ->
  m ()
sendLinkTroughChannelProvided mbPaymentLink driverId mbAmount mbChannel sendDeepLink mkey = do
  driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
      merchantOpCityId = driver.merchantOperatingCityId
      merchantId = driver.merchantId
  webPaymentLink <- getPaymentLinks mbPaymentLink driverId sendDeepLink
  case mbChannel of
    Just MessageKey.WHATSAPP -> do
      merchantMessage <- QMM.findByMerchantOpCityIdAndMessageKey merchantOpCityId mkey >>= fromMaybeM (MerchantMessageNotFound merchantOpCityId.getId (show mkey))
      let amount = show <$> mbAmount
      let whatsAppReq =
            Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq
              { sendTo = phoneNumber,
                templateId = merchantMessage.templateId,
                var1 = amount,
                var2 = Nothing,
                var3 = Nothing,
                ctaButtonUrl = Just webPaymentLink,
                containsUrlButton = Just merchantMessage.containsUrlButton
              }
      result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI merchantId merchantOpCityId whatsAppReq
      when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")
    Just MessageKey.SMS -> do
      smsCfg <- asks (.smsCfg)
      message <-
        MessageBuilder.buildSendPaymentLink merchantOpCityId $
          MessageBuilder.BuildSendPaymentLinkReq
            { paymentLink = webPaymentLink,
              amount = show (fromMaybe 0 mbAmount)
            }
      Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber smsCfg.sender)
        >>= Sms.checkSmsResult
    _ -> pure ()
  return ()

getPaymentLinks :: (MonadFlow m) => Maybe Payment.PaymentLinks -> Id DP.Person -> Bool -> m Text
getPaymentLinks mbPaymentLink driverId sendDeepLink = do
  let mbDeepLink = if sendDeepLink then mbPaymentLink >>= (.deep_link) else Nothing
  case (mbPaymentLink >>= (.web), mbDeepLink) of
    (_, Just deepLink) -> return deepLink
    (Just paymentLink, Nothing) -> return $ showBaseUrl paymentLink
    (Nothing, Nothing) -> throwError $ InternalError ("No payment link found for driverId" <> show driverId.getId)

data DeepLinkData = DeepLinkData
  { sendDeepLink :: Maybe Bool,
    expiryTimeInMinutes :: Maybe Int
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON, Show, Ord, Eq)
