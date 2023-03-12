{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.QuoteSpecialZone where

import Data.Int (Int32)

import Domain.Types.QuoteSpecialZone
import Domain.Types.SearchRequestSpecialZone
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
-- import Kernel.Types.Common
import qualified Storage.Tabular.FareParameters as Fare

-- import Storage.Queries.FullEntityBuilders
import Storage.Tabular.QuoteSpecialZone

create :: QuoteSpecialZone -> SqlDB ()
create quote = Esq.runTransaction $
  withFullEntity quote $ \(quoteT, fareParamsT) -> do
    Esq.create' fareParamsT
    Esq.create' quoteT

-- findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
-- findAllByRequestId requestId = Esq.buildDType $ do
--   quoteT <- Esq.findAll' $ do
--     quote <- from $ table @QuoteT
--     where_ $ quote ^. QuoteRequestId ==. val (toKey requestId)
--     return quote
--   catMaybes <$> mapM buildFullQuote quoteT


-- findAllByRequestId :: Transactionable m => Id SearchRequestSpecialZone -> m [QuoteSpecialZone]
-- findAllByRequestId searchReqId = do
--   buildDType $ do
--     fmap (fmap $ extractSolidType QuoteSpecialZone) $
--       Esq.findAll' $ do
--         (dQuote :& farePars) <-
--           from baseQuoteSpecialZoneQuery
--         where_ $
--           dQuote ^. QuoteSpecialZoneSearchRequestId ==. val (toKey searchReqId)
        -- pure (dQuote, farePars)

countAllByRequestId :: Transactionable m => Id SearchRequestSpecialZone -> m Int32
countAllByRequestId searchReqId = do
  fmap (fromMaybe 0) $
    Esq.findOne $ do
      dQuote <- from $ table @QuoteSpecialZoneT
      where_ $
        dQuote ^.QuoteSpecialZoneSearchRequestId ==. val (toKey searchReqId)
      pure (countRows @Int32)

baseQuoteSpecialZoneQuery ::
  From
    ( SqlExpr (Entity QuoteSpecialZoneT)
        :& SqlExpr (Entity Fare.FareParametersT)
    )
baseQuoteSpecialZoneQuery =
  table @QuoteSpecialZoneT
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& farePars) ->
                   rb ^. QuoteSpecialZoneFareParametersId ==. farePars ^. Fare.FareParametersTId
               )


findById :: (Transactionable m) => Id QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
findById dQuoteId = buildDType $
  fmap (fmap $ extractSolidType @QuoteSpecialZone) $
    Esq.findOne' $ do
      (dQuote :& farePars) <-
        from baseQuoteSpecialZoneQuery
      where_ $ dQuote ^. QuoteSpecialZoneTId ==. val (toKey dQuoteId)
      pure (dQuote, farePars)

-- findById :: Transactionable m => Id QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
-- findById quoteId = buildDType $
--   fmap (fmap $ extractSolidType @Domain.DriverQuote) $
--     Esq.findOne' $ do
--       (dQuote :& farePars) <-
--         from baseDriverQuoteQuery
--       where_ $ dQuote ^. DriverQuoteTId ==. val (toKey dQuoteId)
--       pure (dQuote, farePars)



{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- module Storage.Queries.DriverQuote where

-- import Data.Int (Int32)
-- import qualified Domain.Types.DriverQuote as Domain
-- import Domain.Types.Person
-- import qualified Domain.Types.SearchRequest as DSReq
-- import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
-- import Kernel.Types.Common
-- import Kernel.Types.Id
-- import Kernel.Utils.Common (addUTCTime, secondsToNominalDiffTime)
-- import Storage.Tabular.DriverQuote
-- import qualified Storage.Tabular.FareParameters as Fare

-- create :: Domain.DriverQuote -> SqlDB ()
-- create dQuote = Esq.runTransaction $
--   withFullEntity dQuote $ \(dQuoteT, fareParamsT) -> do
--     Esq.create' fareParamsT
--     Esq.create' dQuoteT

-- baseDriverQuoteQuery ::
--   From
--     ( SqlExpr (Entity DriverQuoteT)
--         :& SqlExpr (Entity Fare.FareParametersT)
--     )
-- baseDriverQuoteQuery =
--   table @DriverQuoteT
--     `innerJoin` table @Fare.FareParametersT
--       `Esq.on` ( \(rb :& farePars) ->
--                    rb ^. DriverQuoteFareParametersId ==. farePars ^. Fare.FareParametersTId
              --  )

-- findById :: (Transactionable m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
-- findById dQuoteId = buildDType $
--   fmap (fmap $ extractSolidType @Domain.DriverQuote) $
--     Esq.findOne' $ do
--       (dQuote :& farePars) <-
--         from baseDriverQuoteQuery
--       where_ $ dQuote ^. DriverQuoteTId ==. val (toKey dQuoteId)
--       pure (dQuote, farePars)

-- setInactiveByRequestId :: Id DSReq.SearchRequest -> SqlDB ()
-- setInactiveByRequestId searchReqId = Esq.update $ \p -> do
--   set p [DriverQuoteStatus =. val Domain.Inactive]
--   where_ $ p ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)

-- findActiveQuotesByDriverId :: (Transactionable m, MonadTime m) => Id Person -> Seconds -> m [Domain.DriverQuote]
-- findActiveQuotesByDriverId driverId driverUnlockDelay = do
--   now <- getCurrentTime
--   buildDType $ do
--     let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
--     fmap (fmap $ extractSolidType @Domain.DriverQuote) $
--       Esq.findAll' $ do
--         (dQuote :& farePars) <-
--           from baseDriverQuoteQuery
--         where_ $
--           dQuote ^. DriverQuoteDriverId ==. val (toKey driverId)
--             &&. dQuote ^. DriverQuoteStatus ==. val Domain.Active
--             &&. dQuote ^. DriverQuoteValidTill >. val (addUTCTime delayToAvoidRaces now)
--         pure (dQuote, farePars)

-- findAllByRequestId :: Transactionable m => Id DSReq.SearchRequest -> m [Domain.DriverQuote]
-- findAllByRequestId searchReqId = do
--   buildDType $ do
--     fmap (fmap $ extractSolidType @Domain.DriverQuote) $
--       Esq.findAll' $ do
--         (dQuote :& farePars) <-
--           from baseDriverQuoteQuery
--         where_ $
--           dQuote ^. DriverQuoteStatus ==. val Domain.Active
--             &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
--         pure (dQuote, farePars)

-- countAllByRequestId :: Transactionable m => Id DSReq.SearchRequest -> m Int32
-- countAllByRequestId searchReqId = do
--   fmap (fromMaybe 0) $
--     Esq.findOne $ do
--       dQuote <- from $ table @DriverQuoteT
--       where_ $
--         dQuote ^. DriverQuoteStatus ==. val Domain.Active
--           &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
--       pure (countRows @Int32)

-- deleteByDriverId :: Id Person -> SqlDB ()
-- deleteByDriverId personId =
--   Esq.delete $ do
--     driverQuotes <- from $ table @DriverQuoteT
--     where_ $ driverQuotes ^. DriverQuoteDriverId ==. val (toKey personId)
