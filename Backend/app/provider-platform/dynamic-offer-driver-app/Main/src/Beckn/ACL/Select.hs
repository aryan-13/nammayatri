{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Select (buildSelectReqV2) where

import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified BecknV2.Utils as Utils
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.Select as DSelect
import Kernel.Prelude hiding (error, setField)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import SharedLogic.CallBAP (mkTxnIdKey)
import Tools.Error
import Tools.Metrics (CoreMetrics)

buildSelectReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    CoreMetrics m,
    CacheFlow m r
  ) =>
  Subscriber.Subscriber ->
  Select.SelectReqV2 ->
  m DSelect.DSelectReq
buildSelectReqV2 subscriber req = do
  let context = req.selectReqContext
  ContextV2.validateContext Context.SELECT context
  now <- getCurrentTime
  bap_id <- context.contextBapId & fromMaybeM (InvalidRequest "Missing bap_id")
  bap_uriText <- context.contextBapUri & fromMaybeM (InvalidRequest "Missing bap_uri")
  bap_uri <- parseBaseUrl bap_uriText
  let order = req.selectReqMessage.confirmReqMessageOrder
  unless (subscriber.subscriber_id == bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  transactionUuid <- context.contextTransactionId & fromMaybeM (InvalidRequest "Missing transaction_id")
  let messageId = UUID.toText messageUuid
      transactionId = UUID.toText transactionUuid
  void $ cacheSelectMessageId messageId transactionId
  item <- case order.orderItems of
    Just [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  let customerExtraFee = getCustomerExtraFeeV2 item.itemTags
      autoAssignEnabled = getAutoAssignEnabledV2 item.itemTags
  fulfillment <- case order.orderFulfillments of
    Just [fulfillment] -> pure fulfillment
    _ -> throwError $ InvalidRequest "There should be only one fulfillment"
  estimateIdText <- fulfillment.fulfillmentId & fromMaybeM (InvalidRequest "Missing fulfillment_id")
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupTime = now,
        autoAssignEnabled = autoAssignEnabled,
        customerExtraFee = customerExtraFee,
        estimateId = Id estimateIdText
      }

getCustomerExtraFeeV2 :: Maybe [Spec.TagGroup] -> Maybe Money
getCustomerExtraFeeV2 tagGroups = do
  tagValue <- Utils.getTagV2 Tag.CUSTOMER_TIP_INFO Tag.CUSTOMER_TIP tagGroups
  customerExtraFee <- readMaybe $ T.unpack tagValue
  Just $ Money customerExtraFee

getAutoAssignEnabledV2 :: Maybe [Spec.TagGroup] -> Bool
getAutoAssignEnabledV2 tagGroups =
  let tagValue = Utils.getTagV2 Tag.AUTO_ASSIGN_ENABLED Tag.IS_AUTO_ASSIGN_ENABLED tagGroups
   in case tagValue of
        Just "True" -> True
        Just "False" -> False
        _ -> False

cacheSelectMessageId :: CacheFlow m r => Text -> Text -> m ()
cacheSelectMessageId messageId transactionId = do
  let msgKey = mkTxnIdKey transactionId
  Hedis.setExp msgKey messageId 3600
