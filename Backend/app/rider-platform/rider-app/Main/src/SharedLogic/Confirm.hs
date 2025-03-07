{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Confirm where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalDetails as DRental
import qualified Domain.Types.SearchRequest as DSReq
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error
import Tools.Event

data DConfirmReq = DConfirmReq
  { personId :: Id DP.Person,
    quoteId :: Id DQuote.Quote,
    paymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod)
  }

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DL.Location,
    toLoc :: Maybe DL.Location,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    booking :: DRB.Booking,
    riderPhone :: Maybe Text,
    riderName :: Maybe Text,
    searchRequestId :: Id DSReq.SearchRequest,
    merchant :: DM.Merchant,
    city :: Context.City,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmInterCityDetails Text
  | ConfirmRentalDetails Text
  | ConfirmAutoDetails Text
  | ConfirmOneWaySpecialZoneDetails Text
  deriving (Show, Generic)

confirm ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    EncFlow m r
  ) =>
  DConfirmReq ->
  m DConfirmRes
confirm DConfirmReq {..} = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  fulfillmentId <-
    case quote.quoteDetails of
      DQuote.OneWayDetails _ -> pure Nothing
      DQuote.RentalDetails rentalDetails -> return $ Just rentalDetails.id.getId
      DQuote.DriverOfferDetails driverOffer -> do
        estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
        when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
        when (driverOffer.validTill < now) $
          throwError $ QuoteExpired quote.id.getId
        pure (Just driverOffer.bppQuoteId)
      DQuote.OneWaySpecialZoneDetails details -> pure (Just details.quoteId)
      DQuote.InterCityDetails details -> pure (Just details.quoteId)
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderId personId
  scheduledBookings <- QRideB.findByRiderIdAndStatus personId [DRB.CONFIRMED]
  let searchDist = round $ fromMaybe 0 $ (.getHighPrecMeters) <$> searchRequest.distance
      searchDur = fromMaybe 0 $ (.getSeconds) <$> searchRequest.estimatedRideDuration
      overlap = any (checkOverlap searchDist searchDur searchRequest.startTime) scheduledBookings
  unless (null activeBooking && not overlap) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  exophone <- findRandomExophone merchantOperatingCityId
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  let isScheduled = merchant.scheduleRideBufferTime `addUTCTime` now < searchRequest.startTime
  booking <- buildBooking searchRequest fulfillmentId quote fromLocation mbToLocation exophone now Nothing paymentMethodId isScheduled
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderPhone <- mapM decrypt person.mobileNumber
  let riderName = person.firstName
  triggerBookingCreatedEvent BookingEventData {booking = booking}
  details <- mkConfirmQuoteDetails quote.quoteDetails fulfillmentId
  paymentMethod <- forM paymentMethodId $ \paymentMethodId' -> do
    paymentMethod <-
      CQMPM.findByIdAndMerchantOperatingCityId paymentMethodId' merchantOperatingCityId
        >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId'.getId)
    unless (paymentMethodId' `elem` searchRequest.availablePaymentMethods) $
      throwError (InvalidRequest "Payment method not allowed")
    pure paymentMethod

  -- DB.runTransaction $ do
  _ <- QRideB.createBooking booking
  _ <- QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill}
  _ <- QEstimate.updateStatusByRequestId quote.requestId DEstimate.COMPLETED
  QPFS.clearCache searchRequest.riderId
  return $
    DConfirmRes
      { booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        itemId = booking.itemId,
        fromLoc = fromLocation,
        toLoc = mbToLocation,
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        searchRequestId = searchRequest.id,
        maxEstimatedDistance = searchRequest.maxDistance,
        paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> paymentMethod,
        ..
      }
  where
    mkConfirmQuoteDetails quoteDetails fulfillmentId = do
      case quoteDetails of
        DQuote.OneWayDetails _ -> pure ConfirmOneWayDetails
        DQuote.RentalDetails DRental.RentalDetails {id} -> pure $ ConfirmRentalDetails id.getId
        DQuote.InterCityDetails details -> pure $ ConfirmInterCityDetails details.quoteId
        DQuote.DriverOfferDetails _ -> do
          bppQuoteId <- fulfillmentId & fromMaybeM (InternalError "FulfillmentId not found in Init. this error should never come.")
          pure $ ConfirmAutoDetails bppQuoteId
        DQuote.OneWaySpecialZoneDetails details -> pure $ ConfirmOneWaySpecialZoneDetails details.quoteId
    checkOverlap :: Int -> Int -> UTCTime -> DRB.Booking -> Bool
    checkOverlap estimatedDistance estimatedDuration curBookingStartTime booking = do
      let estimatedDistanceInKm = estimatedDistance `div` 1000
          estRideEndTimeByDuration = addUTCTime (intToNominalDiffTime estimatedDuration) curBookingStartTime
          estRideEndTimeByDist = addUTCTime (intToNominalDiffTime $ (estimatedDistanceInKm * 3 * 60) + (30 * 60)) curBookingStartTime -- TODO: Make config later
      max estRideEndTimeByDuration estRideEndTimeByDist >= booking.startTime

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  Maybe Text ->
  DQuote.Quote ->
  DL.Location ->
  Maybe DL.Location ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  Bool ->
  m DRB.Booking
buildBooking searchRequest mbFulfillmentId quote fromLoc mbToLoc exophone now otpCode paymentMethodId isScheduled = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        transactionId = searchRequest.id.getId,
        bppBookingId = Nothing,
        fulfillmentId = mbFulfillmentId,
        quoteId = Just quote.id,
        paymentMethodId,
        paymentUrl = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        primaryExophone = exophone.primaryPhone,
        providerUrl = quote.providerUrl,
        itemId = quote.itemId,
        startTime = searchRequest.startTime,
        riderId = searchRequest.riderId,
        fromLocation = fromLoc,
        estimatedFare = quote.estimatedFare,
        discount = quote.discount,
        estimatedTotalFare = quote.estimatedTotalFare,
        estimatedDistance = searchRequest.distance,
        estimatedDuration = searchRequest.estimatedRideDuration,
        vehicleVariant = quote.vehicleVariant,
        bookingDetails,
        tripTerms = quote.tripTerms,
        merchantId = searchRequest.merchantId,
        merchantOperatingCityId = searchRequest.merchantOperatingCityId,
        specialLocationTag = quote.specialLocationTag,
        isScheduled = isScheduled,
        createdAt = now,
        updatedAt = now
      }
  where
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails _ -> pure $ DRB.RentalDetails (DRB.RentalBookingDetails {stopLocation = mbToLoc})
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
      DQuote.InterCityDetails _ -> DRB.InterCityDetails <$> buildInterCityDetails

    buildInterCityDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.InterCityBookingDetails {..}
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DExophone.Exophone
findRandomExophone merchantOperatingCityId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  exophones <- CQExophone.findByMerchantOperatingCityIdAndService merchantOperatingCityId merchantServiceUsageConfig.getExophone
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantOperatingCityId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones
