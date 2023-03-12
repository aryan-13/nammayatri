{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Search where

import Data.List (elemIndex, nubBy)
import Domain.Types.FarePolicy.FarePolicy (FarePolicy)
import qualified Domain.Types.Merchant as DM

import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.QuoteSpecialZone as DQuoteSpecialZone
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude (whenJustM)
import Kernel.External.Maps.Google.PolyLinePoints
import Kernel.Prelude
import Kernel.Serviceability
import Kernel.Storage.Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CacheDistance as CD
import qualified Kernel.Storage.Esqueleto as Esq
import SharedLogic.DriverPool hiding (lat, lon)
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.SearchRequestSpecialZone as QSearchRequestSpecialZone
import qualified Storage.Queries.QuoteSpecialZone as QQuoteSpecialZone
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics
import Domain.Types.FareParameters

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity,
    routeInfo :: Maybe Maps.RouteInfo
  }

data SpecialZoneQuoteInfo = SpecialZoneQuoteInfo
  { quoteId :: Id DQuoteSpecialZone.QuoteSpecialZone,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    -- estimatedTotalFare :: Money,
    fromLocation :: LatLong,
    toLocation :: LatLong,
    startTime :: UTCTime
  }
data DSearchRes = DSearchRes
  { provider :: DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    now :: UTCTime,
    estimateList :: Maybe [EstimateItem],
    specialQuoteList :: Maybe [SpecialZoneQuoteInfo],
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }
data EstimateItem = EstimateItem
  { vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    minFare :: Money,
    maxFare :: Money,
    estimateBreakupList :: [BreakupItem],
    nightShiftRate :: Maybe NightShiftRate,
    waitingCharges :: WaitingCharges,
    driversLatLong :: [LatLong]
  }

data WaitingCharges = WaitingCharges
  { waitingTimeEstimatedThreshold :: Maybe Seconds,
    waitingChargePerMin :: Maybe Money
  }

data NightShiftRate = NightShiftRate
  { nightShiftMultiplier :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DM.Merchant,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

data BreakupItem = BreakupItem
  { title :: Text,
    price :: BreakupPrice
  }

data BreakupPrice = BreakupPrice
  { currency :: Text,
    value :: Money
  }

data DistanceAndDuration = DistanceAndDuration
  { distance :: Meters,
    duration :: Seconds
  }

isSpecialZone :: Bool
isSpecialZone = True

getDistanceAndDuration :: Id DM.Merchant -> DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> Maybe Maps.RouteInfo -> Flow DistanceAndDuration
getDistanceAndDuration merchantId fromLocation toLocation routeInfo = case routeInfo of
  Just (Maps.RouteInfo (Just duration) (Just distance) _ _ _) -> return $ DistanceAndDuration {distance, duration}
  _ -> getMapsDistance
  where
    getMapsDistance = do
      response <-
        Maps.getDistance merchantId $
          Maps.GetDistanceReq
            { origin = fromLocation,
              destination = toLocation,
              travelMode = Just Maps.CAR
            }
      return DistanceAndDuration {distance = response.distance, duration = response.duration}

handler :: Id DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchantId sReq = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless org.enabled $ throwError AgencyDisabled
  searchMetricsMVar <- Metrics.startSearchMetrics org.name
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  let pickupLatLong = Maps.getCoordinates fromLocation
  let mbDropoffLatLong = Just $ Maps.getCoordinates toLocation
  unlessM (rideServiceable org.geofencingConfig QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable
  result <- getDistanceAndDuration merchantId fromLocation toLocation sReq.routeInfo
  CD.cacheDistance sReq.transactionId (result.distance, result.duration)
  logDebug $ "distance: " <> show result.distance
  allFarePolicies <- FarePolicyS.findAllByMerchantId org.id (Just result.distance)
  let farePolicies = filter (checkTripConstraints result.distance) allFarePolicies

  (quotes :: Maybe [SpecialZoneQuoteInfo], estimates':: Maybe [EstimateItem]) <- 
    if isSpecialZone then do
      whenJustM
        (QSearchRequestSpecialZone.findByMsgIdAndBapIdAndBppId sReq.messageId sReq.bapId merchantId)
        (\_ -> throwError $ InvalidRequest "Duplicate Search request")
      searchRequestSpecialZone <- buildSearchRequestSpecialZone sReq merchantId fromLocation toLocation result.distance result.duration
      Esq.runTransaction $ do
        QSearchRequestSpecialZone.create searchRequestSpecialZone
      now <- getCurrentTime
      let listOfVehicleVariants =
            catMaybes $
              everyPossibleVariant <&> \var ->
                find ((== var) . (.vehicleVariant)) farePolicies
      listOfSpecialZoneQuotes <-
        for listOfVehicleVariants $ \farePolicy -> do
          -- calculateFare org.id farePolicy dist startTime Nothing
          fareParams <- calculateFare org.id farePolicy result.distance sReq.pickupTime Nothing
          buildSpecialZoneQuote
            searchRequestSpecialZone
            fareParams
            org.id
            result.distance
            -- poolResult.distanceToPickup
            farePolicy.vehicleVariant
            result.duration --estimatedRideFinishTime need to construct
      Esq.runTransaction $
        for_ listOfSpecialZoneQuotes QQuoteSpecialZone.create
      pure (Just (mkQuoteInfo fromLocation toLocation now <$> listOfSpecialZoneQuotes), Nothing)
    -- pure ()
    else do
      estimates <-
        if null farePolicies
          then do
            logDebug "Trip doesnot match any fare policy constraints."
            return []
          else do
            driverPoolCfg <- getDriverPoolConfig result.distance
            driverPool <- calculateDriverPool Estimate driverPoolCfg Nothing pickupLatLong org.id True Nothing

            logDebug $ "Search handler: driver pool " <> show driverPool

            let listOfProtoQuotes = nubBy ((==) `on` (.variant)) driverPool
                filteredProtoQuotes = zipMatched farePolicies listOfProtoQuotes
            estimates <- mapM (buildEstimate org sReq.pickupTime result.distance driverPool) filteredProtoQuotes
            logDebug $ "bap uri: " <> show sReq.bapUri
            return estimates
      return (Nothing, Just estimates)
  buildSearchRes org fromLocation toLocation estimates' quotes searchMetricsMVar
  where
    checkTripConstraints tripDistance fp =
      let cond1 = (<= tripDistance) <$> fp.minAllowedTripDistance
          cond2 = (>= tripDistance) <$> fp.maxAllowedTripDistance
       in and $ catMaybes [cond1, cond2]

    zipMatched farePolicies driverPool =
      mapMaybe match farePolicies
      where
        match :: FarePolicy -> Maybe (FarePolicy, DriverPoolResult)
        match farePolicy =
          let fpVehicleVariant = farePolicy.vehicleVariant
              driverPoolVariants = map (.variant) driverPool
              midx = elemIndex fpVehicleVariant driverPoolVariants
           in fmap (\idx -> (farePolicy, driverPool !! idx)) midx

buildEstimate ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  UTCTime ->
  Meters ->
  [DriverPoolResult] ->
  (FarePolicy, DriverPoolResult) ->
  m EstimateItem
buildEstimate org startTime dist driverpool (farePolicy, driverMetadata) = do
  fareParams <- calculateFare org.id farePolicy dist startTime Nothing
  let estimateFare = fareSum fareParams
      currency = "INR"
      estimateBreakups = mkBreakupListItems (BreakupPrice currency) BreakupItem farePolicy
  let filteredPoolByVehVariant = filter (\pool -> pool.variant == driverMetadata.variant) driverpool
  let latlongList = map (\x -> LatLong x.lat x.lon) filteredPoolByVehVariant
  logDebug $ "estimateFare: " <> show estimateFare
  logDebug $ "distanceToPickup: " <> show driverMetadata.distanceToPickup
  pure
    EstimateItem
      { vehicleVariant = driverMetadata.variant,
        distanceToPickup = driverMetadata.distanceToPickup,
        minFare = estimateFare,
        maxFare = estimateFare + farePolicy.driverExtraFee.maxFee,
        estimateBreakupList = estimateBreakups,
        driversLatLong = latlongList,
        nightShiftRate =
          Just $
            NightShiftRate
              { nightShiftMultiplier = fareParams.nightShiftRate,
                nightShiftStart = farePolicy.nightShiftStart,
                nightShiftEnd = farePolicy.nightShiftEnd
              },
        waitingCharges =
          WaitingCharges
            { waitingTimeEstimatedThreshold = farePolicy.waitingTimeEstimatedThreshold,
              waitingChargePerMin = farePolicy.waitingChargePerMin
            }
      }

mkBreakupListItems ::
  (Money -> breakupItemPrice) ->
  (Text -> breakupItemPrice -> breakupItem) ->
  FarePolicy ->
  [breakupItem]
mkBreakupListItems mkPrice mkBreakupItem farePolicy = do
  let baseDistanceFare = roundToIntegral farePolicy.baseDistanceFare
      baseDistanceFareCaption = "BASE_DISTANCE_FARE"
      baseDistanceFareItem = mkBreakupItem baseDistanceFareCaption (mkPrice baseDistanceFare)

      perExtraKmFare = roundToIntegral farePolicy.perExtraKmFare
      perExtraKmFareCaption = "EXTRA_PER_KM_FARE"
      perExtraKmFareItem = mkBreakupItem perExtraKmFareCaption (mkPrice perExtraKmFare)

      deadKmFare = farePolicy.deadKmFare
      deadKmFareCaption = "DEAD_KILOMETER_FARE"
      deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice deadKmFare)

      driverMinExtraFee = farePolicy.driverExtraFee.minFee
      driverMinExtraFeeCaption = "DRIVER_MIN_EXTRA_FEE"
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption (mkPrice driverMinExtraFee)

      driverMaxExtraFee = farePolicy.driverExtraFee.maxFee
      driverMaxExtraFeeCaption = "DRIVER_MAX_EXTRA_FEE"
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption (mkPrice driverMaxExtraFee)

  [baseDistanceFareItem, perExtraKmFareItem, deadKmFareItem, driverMinExtraFeeItem, driverMaxExtraFeeItem]

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

buildSearchRes ::
  (MonadTime m) =>
  DM.Merchant ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Maybe [EstimateItem] ->
  Maybe [SpecialZoneQuoteInfo] ->
  Metrics.SearchMetricsMVar ->
  m DSearchRes
buildSearchRes org fromLocation toLocation estimateList specialQuoteList searchMetricsMVar = do
  now <- getCurrentTime
  pure $
    DSearchRes
      { provider = org,
        now,
        fromLocation,
        toLocation,
        estimateList,
        specialQuoteList,
        searchMetricsMVar
      }


buildSearchRequestSpecialZone ::
  ( MonadGuid m,
    MonadTime m,
    MonadReader r m,
  HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DSearchReq ->
  Id DM.Merchant ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Meters ->
  Seconds -> 
  m DSRSZ.SearchRequestSpecialZone
buildSearchRequestSpecialZone DSearchReq {..} providerId fromLocation toLocation estimatedDistance estimatedDuration = do
  uuid <- generateGUID
  now <- getCurrentTime
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DSRSZ.SearchRequestSpecialZone
      { id = Id uuid,
        startTime = pickupTime,
        createdAt = now,
        updatedAt = now,
        ..
      }

buildSpecialZoneQuote ::
  EsqDBFlow m r =>
  DSRSZ.SearchRequestSpecialZone ->
  FareParameters ->
  Id DM.Merchant ->
  Meters ->
  -- Meters ->
  DVeh.Variant ->
  Seconds ->
  m DQuoteSpecialZone.QuoteSpecialZone
buildSpecialZoneQuote productSearchRequest fareParams transporterId distance vehicleVariant duration = do
  quoteId <- Id <$> generateGUID
  now <- getCurrentTime
  let estimatedFare = fareSum fareParams
      estimatedFinishTime = fromIntegral duration `addUTCTime` now
      -- discount = fareParams.discount
      -- estimatedTotalFare = fareSumWithDiscount fareParams
  -- let oneWayQuoteDetails = DQuote.OneWayQuoteDetails {..}
  pure
    DQuoteSpecialZone.QuoteSpecialZone
      { id = quoteId,
        searchRequestId = productSearchRequest.id,
        providerId = transporterId,
        createdAt = now,
        updatedAt = now,
        validTill = now,
        -- quoteDetails = DQuote.OneWayDetails oneWayQuoteDetails,
        ..
      }

-- data QuoteSpecialZone = QuoteSpecialZone
--   { id :: Id QuoteSpecialZone,
--     -- status :: DriverQuoteStatus,
--     searchRequestId :: Id SearchRequestSpecialZone,
--     -- searchRequestForDriverId :: Maybe (Id SearchRequestForDriver),
--     -- driverId :: Id Person,
--     -- driverName :: Text,
--     -- driverRating :: Maybe Centesimal,
--     vehicleVariant :: Variant.Variant,
--     distance :: Meters,
--     estimatedFinishTime :: UTCTime,
--     -- distanceToPickup :: Meters,
--     -- durationToPickup :: Seconds,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime,
--     validTill :: UTCTime,
--     estimatedFare :: Money,
--     fareParams :: Params.FareParameters
--   }


mkQuoteInfo :: DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> UTCTime -> DQuoteSpecialZone.QuoteSpecialZone -> SpecialZoneQuoteInfo
mkQuoteInfo fromLoc toLoc startTime DQuoteSpecialZone.QuoteSpecialZone {..} = do
  let fromLocation = Maps.getCoordinates fromLoc
      toLocation = Maps.getCoordinates toLoc
      -- distanceToNearestDriver = case quoteDetails of
      --   DQuote.OneWayDetails details -> DQuote.distanceToNearestDriver details
      --   _ -> 0
  SpecialZoneQuoteInfo
    { quoteId = id,
      ..
    }