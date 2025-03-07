{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Serviceability
  ( checkServiceability,
    getNearestOperatingAndCurrentCity,
    ServiceabilityRes (..),
    NearestOperatingAndCurrentCity (..),
    CityState (..),
  )
where

import API.UI.HotSpot
import Data.List (sortBy)
import Data.Ord
import qualified Domain.Types.HotSpot as DHotSpot
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import Kernel.Beam.Functions
import Kernel.External.Maps.Types hiding (geometry)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Queries.SpecialLocation as QSpecialLocation
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CRiderConfig
import qualified Storage.CachedQueries.Person as CQP
import Storage.Queries.Geometry (findGeometriesContaining)
import Tools.Error

data ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool,
    city :: Maybe Context.City,
    currentCity :: Maybe Context.City,
    specialLocation :: Maybe QSpecialLocation.SpecialLocationFull,
    geoJson :: Maybe Text,
    hotSpotInfo :: [DHotSpot.HotSpotInfo],
    blockRadius :: Maybe Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

checkServiceability ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  LatLong ->
  Bool ->
  m ServiceabilityRes
checkServiceability settingAccessor (personId, merchantId) location shouldUpdatePerson = do
  DHotSpot.HotSpotResponse {..} <- getHotspot location merchantId
  mbNearestOpAndCurrentCity <- getNearestOperatingAndCurrentCity' settingAccessor (personId, merchantId) shouldUpdatePerson location
  case mbNearestOpAndCurrentCity of
    Just (NearestOperatingAndCurrentCity {nearestOperatingCity, currentCity}) -> do
      let city = Just nearestOperatingCity.city
      merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId nearestOperatingCity.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId:- " <> merchantId.getId <> " ,city:- " <> show nearestOperatingCity.city)
      cityConfig <- CRiderConfig.findByMerchantOperatingCityId merchantOperatingCity.id
      specialLocationBody <- QSpecialLocation.findSpecialLocationByLatLongFull location $ maybe 150 (.specialZoneRadius) cityConfig -- default as 150 meters
      return ServiceabilityRes {serviceable = True, currentCity = Just currentCity.city, specialLocation = specialLocationBody, geoJson = (.geoJson) =<< specialLocationBody, ..}
    Nothing -> return ServiceabilityRes {city = Nothing, currentCity = Nothing, serviceable = False, specialLocation = Nothing, geoJson = Nothing, ..}

data NearestOperatingAndCurrentCity = NearestOperatingAndCurrentCity
  { nearestOperatingCity :: CityState,
    currentCity :: CityState
  }

data CityState = CityState
  { city :: Context.City,
    state :: Context.IndianState
  }

getNearestOperatingAndCurrentCity ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  Bool ->
  LatLong ->
  m NearestOperatingAndCurrentCity
getNearestOperatingAndCurrentCity settingAccessor (personId, merchantId) shouldUpdatePerson latLong = do
  mbNearestOpAndCurrentCity <- getNearestOperatingAndCurrentCity' settingAccessor (personId, merchantId) shouldUpdatePerson latLong
  case mbNearestOpAndCurrentCity of
    Just a -> return a
    Nothing -> throwError RideNotServiceable

getNearestOperatingAndCurrentCity' ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  (Id Person.Person, Id Merchant.Merchant) ->
  Bool ->
  LatLong ->
  m (Maybe NearestOperatingAndCurrentCity)
getNearestOperatingAndCurrentCity' settingAccessor (personId, merchantId) shouldUpdatePerson latLong = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let merchantCityState = CityState {city = merchant.defaultCity, state = merchant.defaultState}
  let geoRestriction = settingAccessor (merchant.geofencingConfig)
  mbNearestOpAndCurrentCity <- do
    case geoRestriction of
      Unrestricted -> do
        return $ Just $ NearestOperatingAndCurrentCity {nearestOperatingCity = merchantCityState, currentCity = merchantCityState}
      Regions regions -> do
        {-
          Below logic is to find the nearest operating city for the pickup location.
          If the pickup location is in the operating city, then return the city.
          If the pickup location is not in the city, then return the nearest city for that state else the merchant default city.
        -}
        geoms <- runInReplica $ findGeometriesContaining latLong regions
        case filter (\geom -> geom.city /= Context.AnyCity) geoms of
          [] ->
            find (\geom -> geom.city == Context.AnyCity) geoms & \case
              Just anyCityGeom -> do
                cities <- CQMOC.findAllByMerchantIdAndState merchant.id anyCityGeom.state >>= mapM (\m -> return (distanceBetweenInMeters latLong (LatLong m.lat m.long), m.city))
                let nearestOperatingCity = maybe merchantCityState (\p -> CityState {city = snd p, state = anyCityGeom.state}) (listToMaybe $ sortBy (comparing fst) cities)
                return $ Just $ NearestOperatingAndCurrentCity {currentCity = CityState {city = anyCityGeom.city, state = anyCityGeom.state}, nearestOperatingCity}
              Nothing -> do
                logError $ "No geometry found for latLong: " <> show latLong <> " for regions: " <> show regions
                return Nothing
          (g : _) -> do
            -- Nearest operating city and source city are same
            let operatingCityState = CityState {city = g.city, state = g.state}
            return $ Just $ NearestOperatingAndCurrentCity {nearestOperatingCity = operatingCityState, currentCity = operatingCityState}
  whenJust mbNearestOpAndCurrentCity $ \NearestOperatingAndCurrentCity {nearestOperatingCity} -> do
    upsertPersonCityInformation personId merchantId shouldUpdatePerson (Just nearestOperatingCity.city)
  return mbNearestOpAndCurrentCity

upsertPersonCityInformation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> Id Merchant.Merchant -> Bool -> Maybe Context.City -> m ()
upsertPersonCityInformation personId merchantId shouldUpdatePerson mbCity = when shouldUpdatePerson $
  whenJust mbCity $ \city' -> do
    personCityInfo <- CQP.findCityInfoById personId >>= fromMaybeM (PersonCityInformationDoesNotExist $ "personId:- " <> personId.getId)
    when (personCityInfo.currentCity /= city') $ do
      merchantOperatingCity <-
        CQMOC.findByMerchantIdAndCity merchantId city'
          >>= fromMaybeM
            ( MerchantOperatingCityNotFound $
                "merchantId:- " <> merchantId.getId <> " ,city:- " <> show city'
            )
      CQP.updateCityInfoById personId city' merchantOperatingCity.id
