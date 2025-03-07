{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.FareProduct where

import qualified Domain.Types.Common as DTC
import Domain.Types.FareProduct
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import qualified Storage.Queries.FareProduct as Queries

create :: (MonadFlow m, Esq.EsqDBFlow m r, CacheFlow m r) => FareProduct -> m ()
create = Queries.create

findAllUnboundedFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Area -> m [FareProduct]
findAllUnboundedFareProductForVariants merchantOpCityId tripCategory area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea merchantOpCityId tripCategory area /=<< Queries.findAllUnboundedFareProductForVariants merchantOpCityId tripCategory area

cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Area -> [FareProduct] -> m ()
cacheAllUnboundedFareProductForVariantsByMerchantIdAndArea merchantOpCityId tripCategory area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area) fareProducts expTime

makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey :: Id MerchantOperatingCity -> DTC.TripCategory -> Area -> Text
makeUnboundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllBoundedFareProductForVariants :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Area -> m [FareProduct]
findAllBoundedFareProductForVariants merchantOpCityId tripCategory area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeBoundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area) >>= \case
    Just a -> pure a
    Nothing -> cacheAllBoundedFareProductForVariantsByMerchantIdAndArea merchantOpCityId tripCategory area /=<< Queries.findAllBoundedFareProductForVariants merchantOpCityId tripCategory area

cacheAllBoundedFareProductForVariantsByMerchantIdAndArea :: (CacheFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Area -> [FareProduct] -> m ()
cacheAllBoundedFareProductForVariantsByMerchantIdAndArea merchantOpCityId tripCategory area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeBoundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area) fareProducts expTime

makeBoundedFareProductForVariantsByMerchantIdAndAreaKey :: Id MerchantOperatingCity -> DTC.TripCategory -> Area -> Text
makeBoundedFareProductForVariantsByMerchantIdAndAreaKey merchantOpCityId tripCategory area = "driver-offer:CachedQueries:Bounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllFareProductByMerchantOpCityId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> m [FareProduct]
findAllFareProductByMerchantOpCityId merchantOpCityId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeFareProductByMerchantOpCityIdKey merchantOpCityId) >>= \case
    Just a -> pure a
    Nothing -> cacheAllFareProductByMerchantOpCityId merchantOpCityId /=<< Queries.findAllFareProductByMerchantOpCityId merchantOpCityId

cacheAllFareProductByMerchantOpCityId :: (CacheFlow m r) => Id MerchantOperatingCity -> [FareProduct] -> m ()
cacheAllFareProductByMerchantOpCityId merchantOpCityId fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeFareProductByMerchantOpCityIdKey merchantOpCityId) fareProducts expTime

makeFareProductByMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeFareProductByMerchantOpCityIdKey merchantOpCityId = "driver-offer:CachedQueries:FareProducts:MerchantOpCityId-" <> getId merchantOpCityId

----------------------------------------------------------------------------------------------------------------------------------------------------------------

findUnboundedByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> m (Maybe FareProduct)
findUnboundedByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeUnboundedFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust (cacheUnboundedFareProductByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area) /=<< Queries.findUnboundedByMerchantOpCityIdVariantArea merchantOpCityId tripCategory vehicleVariant area

cacheUnboundedFareProductByMerchantVariantArea :: (CacheFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> FareProduct -> m ()
cacheUnboundedFareProductByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area fareProduct = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeUnboundedFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area) fareProduct expTime

makeUnboundedFareProductByMerchantVariantAreaKey :: Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> Text
makeUnboundedFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area = "driver-offer:CachedQueries:Unbounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

findAllBoundedByMerchantVariantArea :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> m [FareProduct]
findAllBoundedByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeBoundedFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area) >>= \case
    Just a -> pure a
    Nothing -> cacheBoundedFareProductByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area /=<< Queries.findAllBoundedByMerchantOpCityIdVariantArea merchantOpCityId tripCategory vehicleVariant area

cacheBoundedFareProductByMerchantVariantArea :: (CacheFlow m r) => Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> [FareProduct] -> m ()
cacheBoundedFareProductByMerchantVariantArea merchantOpCityId tripCategory vehicleVariant area fareProducts = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeBoundedFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area) fareProducts expTime

makeBoundedFareProductByMerchantVariantAreaKey :: Id MerchantOperatingCity -> DTC.TripCategory -> Variant -> Area -> Text
makeBoundedFareProductByMerchantVariantAreaKey merchantOpCityId tripCategory vehicleVariant area = "driver-offer:CachedQueries:Bounded:FareProduct:MerchantOpCityId-" <> getId merchantOpCityId <> ":Variant-" <> show vehicleVariant <> ":Area-" <> show area <> ":TripCategory-" <> show tripCategory
