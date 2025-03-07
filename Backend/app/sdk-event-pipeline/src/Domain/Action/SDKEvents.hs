{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.SDKEvents where

import Domain.Types.SDKEvents
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.SlidingWindowLimiter
import Servant
import Tools.Auth

postSdkEvents :: SDKEventsReq -> Flow APISuccess
postSdkEvents SDKEventsReq {..} = do
  --   apiRateLimitOptions <- asks (.apiRateLimitOptions)
  --   checkSlidingWindowLimitWithOptions ("rate-limit:" <> personId) apiRateLimitOptions
  let clientType = DRIVER
  case clientType of
    RIDER -> do
      riderSDKEventsKafkaTopic <- asks (.riderSDKEventsKafkaTopic)
      produceMessage (riderSDKEventsKafkaTopic, Nothing) event
    DRIVER -> do
      driverSDKEventsKafkaTopic <- asks (.driverSDKEventsKafkaTopic)
      produceMessage (driverSDKEventsKafkaTopic, Nothing) event
  return Success
