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

module Storage.Beam.Booking where

import qualified Database.Beam as B
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data BookingT f = BookingT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    quoteId :: B.C f Text,
    status :: B.C f Domain.BookingStatus,
    bookingType :: B.C f Domain.BookingType, -- Just for backward compatibilty
    tripCategory :: B.C f (Maybe DTC.TripCategory),
    specialLocationTag :: B.C f (Maybe Text),
    specialZoneOtpCode :: B.C f (Maybe Text),
    disabilityTag :: B.C f (Maybe Text),
    area :: B.C f (Maybe FareProductD.Area),
    providerId :: B.C f Text,
    merchantOperatingCityId :: B.C f (Maybe Text),
    primaryExophone :: B.C f Text,
    bapId :: B.C f Text,
    bapUri :: B.C f Text,
    bapCity :: B.C f (Maybe Context.City),
    bapCountry :: B.C f (Maybe Context.Country),
    startTime :: B.C f UTCTime,
    riderId :: B.C f (Maybe Text),
    fromLocationId :: B.C f (Maybe Text),
    toLocationId :: B.C f (Maybe Text),
    vehicleVariant :: B.C f Veh.Variant,
    estimatedDistance :: B.C f (Maybe Meters),
    maxEstimatedDistance :: B.C f (Maybe HighPrecMeters),
    estimatedFare :: B.C f HighPrecMoney,
    estimatedDuration :: B.C f (Maybe Seconds),
    fareParametersId :: B.C f Text,
    riderName :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    paymentMethodId :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    stopLocationId :: B.C f (Maybe Text),
    distanceToPickup :: B.C f (Maybe HighPrecMeters),
    isScheduled :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Booking = BookingT Identity

$(enableKVPG ''BookingT ['id] [['specialZoneOtpCode], ['quoteId], ['transactionId]])

$(mkTableInstances ''BookingT "booking")
