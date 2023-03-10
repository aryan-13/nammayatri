{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.MerchantAccess where

import qualified Domain.Types.MerchantAccess as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantAccessT sql=merchant_access
      id Text
      personId PersonTId
      merchantId MerchantTId
      createdAt UTCTime
      Primary id
      Unique (MerchantAccessPersonId, MerchantAccessMerchantId)
      deriving Generic
    |]

instance TEntityKey MerchantAccessT where
  type DomainKey MerchantAccessT = Id Domain.MerchantAccess
  fromKey (MerchantAccessTKey _id) = Id _id
  toKey (Id id) = MerchantAccessTKey id

instance FromTType MerchantAccessT Domain.MerchantAccess where
  fromTType MerchantAccessT {..} = do
    return $
      Domain.MerchantAccess
        { id = Id id,
          merchantId = fromKey merchantId,
          personId = fromKey personId,
          ..
        }

instance ToTType MerchantAccessT Domain.MerchantAccess where
  toTType Domain.MerchantAccess {..} =
    MerchantAccessT
      { id = getId id,
        merchantId = toKey merchantId,
        personId = toKey personId,
        ..
      }
