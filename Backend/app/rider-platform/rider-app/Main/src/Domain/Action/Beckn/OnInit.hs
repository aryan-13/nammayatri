{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnInit where

import qualified Database.Esqueleto.Experimental as Esq
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.LocationAddress as DBL
import qualified Domain.Types.RecurringBooking as DRRB
import GHC.Records.Extra (HasField (..))
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RecurringBooking as QRideRB
import qualified Storage.Tabular.Booking as SBooking
import Tools.Error

data OnInitReq
  = OnInitReqBooking OnInitBooking
  | OnInitReqRecurringBooking OnInitRecurringBooking
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

instance HasField "bppBookingId" OnInitReq (Id DRB.BPPBooking) where
  hasField (OnInitReqBooking req) = (\v -> OnInitReqBooking req {bppBookingId = v}, req.bppBookingId)
  hasField (OnInitReqRecurringBooking req) = (\v -> OnInitReqRecurringBooking req {bppBookingId = v}, req.bppBookingId)

data OnInitBooking = OnInitBooking
  { bookingId :: Id DRB.Booking,
    bppBookingId :: Id DRB.BPPBooking,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OnInitRecurringBooking = OnInitRecurringBooking
  { recurringBookingId :: Id DRRB.RecurringBooking,
    bppBookingId :: Id DRB.BPPBooking,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    startTime :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OnInitRes = OnInitRes
  { bookingId :: Id DRB.Booking,
    bppBookingId :: Id DRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocationAddress :: DBL.LocationAddress,
    mbToLocationAddress :: Maybe DBL.LocationAddress,
    estimatedTotalFare :: Money,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text,
    mbRiderName :: Maybe Text
  }
  deriving (Generic, Show, PrettyShow)

onInit :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => BaseUrl -> OnInitReq -> m OnInitRes
onInit registryUrl (OnInitReqBooking req) = do
  bookingOld <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById bookingOld.merchantId >>= fromMaybeM (MerchantNotFound bookingOld.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  DB.runTransaction $ do
    QRideB.updateBPPBookingId req.bookingId req.bppBookingId
    QRideB.updatePaymentInfo req.bookingId req.estimatedFare req.discount req.estimatedTotalFare
  booking <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  decRider <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId) >>= decrypt
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let fromLocation = booking.fromLocation
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.RecurringDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
  return $
    OnInitRes
      { bookingId = booking.id,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocationAddress = fromLocation.address,
        mbToLocationAddress = mbToLocation <&> (.address),
        mbRiderName = decRider.firstName,
        ..
      }
onInit registryUrl (OnInitReqRecurringBooking req) = do
  recurringBooking <- QRideRB.findById req.recurringBookingId >>= fromMaybeM (BookingDoesNotExist req.recurringBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById recurringBooking.merchantId >>= fromMaybeM (MerchantNotFound recurringBooking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  bookingId <- generateGUID
  now <- getCurrentTime
  let bookingT =
        SBooking.BookingT
          { id = bookingId,
            fareProductType = DQuote.DRIVER_OFFER,
            bppBookingId = Just $ req.bppBookingId.getId,
            status = DRB.NEW,
            providerId = recurringBooking.providerId,
            providerUrl = showBaseUrl recurringBooking.providerUrl,
            providerName = recurringBooking.providerName,
            providerMobileNumber = recurringBooking.providerMobileNumber,
            startTime = req.startTime,
            riderId = DB.toKey recurringBooking.riderId,
            fromLocationId = DB.toKey recurringBooking.fromLocation.id,
            toLocationId = Just $ DB.toKey recurringBooking.toLocation.id,
            estimatedFare = realToFrac req.estimatedFare,
            discount = realToFrac <$> req.discount,
            estimatedTotalFare = realToFrac req.estimatedTotalFare,
            distance = Just recurringBooking.distance,
            vehicleVariant = recurringBooking.vehicleVariant,
            tripTermsId = Nothing,
            rentalSlabId = Nothing,
            merchantId = DB.toKey recurringBooking.merchantId,
            createdAt = now,
            updatedAt = now,
            quoteId = Nothing
          }
  DB.runTransaction $ DB.SqlDB . lift $ Esq.insert_ bookingT
  decRider <- QP.findById recurringBooking.riderId >>= fromMaybeM (PersonNotFound recurringBooking.riderId.getId) >>= decrypt
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  return $
    OnInitRes
      { bookingId = Id bookingId,
        bppBookingId = req.bppBookingId,
        bppId = recurringBooking.providerId,
        bppUrl = recurringBooking.providerUrl,
        estimatedTotalFare = req.estimatedTotalFare,
        fromLocationAddress = recurringBooking.fromLocation.address,
        mbToLocationAddress = Just recurringBooking.toLocation.address,
        mbRiderName = decRider.firstName,
        ..
      }
