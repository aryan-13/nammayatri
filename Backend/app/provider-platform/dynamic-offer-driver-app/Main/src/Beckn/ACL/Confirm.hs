{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Confirm where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as Utils
import qualified BecknV2.Utils as Utils
import qualified Data.Text as T
import Domain.Action.Beckn.Confirm as DConfirm
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Field
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing

buildConfirmReqV2 ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Spec.ConfirmReq ->
  Bool ->
  m DConfirm.DConfirmReq
buildConfirmReqV2 req isValueAddNP = do
  Utils.validateContext Context.CONFIRM req.confirmReqContext
  bookingId <- fmap Id req.confirmReqMessage.confirmReqMessageOrder.orderId & fromMaybeM (InvalidRequest "orderId not found")
  fulfillment <- req.confirmReqMessage.confirmReqMessageOrder.orderFulfillments >>= listToMaybe & fromMaybeM (InvalidRequest "Fulfillment not found")
  customerPhoneNumber <- fulfillment.fulfillmentCustomer >>= (.customerContact) >>= (.contactPhone) & fromMaybeM (InvalidRequest "Customer Phone not found")
  let customerMobileCountryCode = "+91" -- TODO: check how to get countrycode via ONDC
  fromAddress' <- fulfillment.fulfillmentStops >>= Utils.getStartLocation >>= (.stopLocation) & maybe (pure Nothing) Utils.parseAddress
  fromAddress <- fromAddress' & fromMaybeM (InvalidRequest "Start location not found")
  let mbRiderName = fulfillment.fulfillmentCustomer >>= (.customerPerson) >>= (.personName)
  let vehCategory = fulfillment.fulfillmentVehicle >>= (.vehicleCategory)
      vehVariant = fulfillment.fulfillmentVehicle >>= (.vehicleVariant)
  vehicleVariant <- Utils.parseVehicleVariant vehCategory vehVariant & fromMaybeM (InvalidRequest $ "Unable to parse vehicle category:-" <> show vehCategory <> ", variant:-" <> show vehVariant)
  let nightSafetyCheck = fulfillment.fulfillmentCustomer >>= (.customerPerson) >>= (.personTags) & getNightSafetyCheckTag isValueAddNP
      enableFrequentLocationUpdates = fulfillment.fulfillmentCustomer >>= (.customerPerson) >>= (.personTags) & getEnableFrequentLocationUpdatesTag isValueAddNP
  toAddress <- fulfillment.fulfillmentStops >>= Utils.getDropLocation >>= (.stopLocation) & maybe (pure Nothing) Utils.parseAddress
  return $
    DConfirm.DConfirmReq
      { ..
      }

getNightSafetyCheckTag :: Bool -> Maybe [Spec.TagGroup] -> Bool
getNightSafetyCheckTag isValueAddNP = maybe isValueAddNP getTagValue
  where
    getTagValue tagGroups =
      let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.NIGHT_SAFETY_CHECK (Just tagGroups)
       in fromMaybe isValueAddNP (readMaybe . T.unpack =<< tagValue)

getEnableFrequentLocationUpdatesTag :: Bool -> Maybe [Spec.TagGroup] -> Bool
getEnableFrequentLocationUpdatesTag isValueAddNP = maybe isValueAddNP getTagValue
  where
    getTagValue tagGroups =
      let tagValue = Utils.getTagV2 Tag.CUSTOMER_INFO Tag.ENABLE_FREQUENT_LOCATION_UPDATES (Just tagGroups)
       in fromMaybe isValueAddNP (readMaybe . T.unpack =<< tagValue)
