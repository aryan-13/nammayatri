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

module Storage.Beam.FarePolicy.FarePolicyProgressiveDetails where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data FarePolicyProgressiveDetailsT f = FarePolicyProgressiveDetailsT
  { farePolicyId :: B.C f Text,
    baseDistance :: B.C f Meters,
    baseFare :: B.C f Money,
    deadKmFare :: B.C f Money,
    waitingCharge :: B.C f (Maybe DPM.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes), -- FIXME typo
    nightShiftCharge :: B.C f (Maybe DPM.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsT where
  data PrimaryKey FarePolicyProgressiveDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyProgressiveDetails = FarePolicyProgressiveDetailsT Identity

$(enableKVPG ''FarePolicyProgressiveDetailsT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyProgressiveDetailsT "fare_policy_progressive_details")
