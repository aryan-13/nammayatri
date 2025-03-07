{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Track
  ( track,
    DTrackReq (..),
    DTrackRes (..),
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide

newtype DTrackReq = TrackReq
  { bookingId :: Id DBooking.Booking
  }

data DTrackRes = TrackRes
  { url :: BaseUrl,
    transporter :: DM.Merchant,
    isRideCompleted :: Bool
  }

track ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  DTrackReq ->
  m DTrackRes
track transporterId req = do
  transporter <-
    QM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  ride <- QRide.findOneByBookingId req.bookingId >>= fromMaybeM (RideDoesNotExist req.bookingId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let isRideCompleted = ride.status == DRide.COMPLETED
  return $
    TrackRes
      { url = ride.trackingUrl,
        ..
      }
