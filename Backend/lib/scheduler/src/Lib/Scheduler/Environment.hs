{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Lib.Scheduler.Environment where

import qualified Data.Map as M
import EulerHS.Interpreters (runFlow)
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import Kernel.Beam.Connection.Flow (prepareConnectionDriver)
import Kernel.Beam.Connection.Types
import Kernel.Beam.Lib.UtilsTH
import Kernel.Beam.Types (KafkaConn (..))
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Beam.SystemConfigs
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis (HedisCfg, HedisEnv, HedisFlow, disconnectHedis)
import Kernel.Storage.Queries.SystemConfigs as QSC
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Flow
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.IOLogging (LoggerEnv, releaseLoggerEnv)
import Lib.Scheduler.Metrics
import Lib.Scheduler.Types

type JobInfoMap = M.Map Text Bool

data SchedulerConfig = SchedulerConfig
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    hedisNonCriticalCfg :: HedisCfg,
    hedisNonCriticalClusterCfg :: HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    hedisPrefix :: Text,
    schedulerType :: SchedulerType,
    schedulerSetName :: Text,
    streamName :: Text,
    groupName :: Text,
    block :: Integer,
    readCount :: Integer,
    port :: Int,
    loopIntervalSec :: Seconds,
    maxThreads :: Int,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    kafkaProducerCfg :: KafkaProducerCfg
  }
  deriving (Generic, FromDhall)

data SchedulerEnv = SchedulerEnv
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    hedisNonCriticalEnv :: HedisEnv,
    hedisNonCriticalClusterEnv :: HedisEnv,
    hedisClusterEnv :: HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
    coreMetrics :: Metrics.CoreMetricsContainer,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    schedulerType :: SchedulerType,
    schedulerSetName :: Text,
    kafkaProducerTools :: KafkaProducerTools,
    streamName :: Text,
    groupName :: Text,
    block :: Integer,
    readCount :: Integer,
    metrics :: SchedulerMetrics,
    loopIntervalSec :: Seconds,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds,
    consumerId :: Text,
    port :: Int,
    isShuttingDown :: Shutdown,
    version :: Metrics.DeploymentVersion,
    enableRedisLatencyLogging :: Bool,
    enablePrometheusMetricLogging :: Bool,
    maxThreads :: Int,
    maxShards :: Int,
    jobInfoMap :: JobInfoMap,
    kvConfigUpdateFrequency :: Int,
    cacheConfig :: CacheConfig
  }
  deriving (Generic)

releaseSchedulerEnv :: SchedulerEnv -> IO ()
releaseSchedulerEnv SchedulerEnv {..} = do
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisNonCriticalEnv
  disconnectHedis hedisNonCriticalClusterEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv

type SchedulerM = FlowR SchedulerEnv

type JobCreatorEnv r = (HasField "jobInfoMap" r (M.Map Text Bool), HasField "maxShards" r Int, HasField "schedulerSetName" r Text)

type JobCreator r m = (JobCreatorEnv r, JobMonad r m)

type JobExecutor r m = (HasField "streamName" r Text, HasField "maxShards" r Int, HasField "groupName" r Text, HasField "schedulerSetName" r Text, JobMonad r m)

type JobMonad r m = (HasField "schedulerType" r SchedulerType, MonadReader r m, HedisFlow m r, MonadFlow m)

runSchedulerM :: HasSchemaName SystemConfigsT => SchedulerConfig -> SchedulerEnv -> SchedulerM a -> IO a
runSchedulerM schedulerConfig env action = do
  let loggerRt = L.getEulerLoggerRuntime Nothing env.loggerConfig
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow
      flowRt
      ( ( prepareConnectionDriver
            ConnectionConfigDriver
              { esqDBCfg = schedulerConfig.esqDBCfg,
                esqDBReplicaCfg = schedulerConfig.esqDBCfg,
                hedisClusterCfg = schedulerConfig.hedisClusterCfg
              }
            env.kvConfigUpdateFrequency
        )
          >> L.setOption KafkaConn env.kafkaProducerTools
      )
    flowRt' <- runFlowR flowRt env $ do
      fork
        "Fetching Kv configs"
        ( forever $ do
            kvConfigs <-
              QSC.findById "kv_configs" >>= pure . decodeFromText' @Tables
                >>= fromMaybeM (InternalError "Couldn't find kv_configs table for scheduler app")
            L.setOption KBT.Tables kvConfigs
            threadDelay (env.kvConfigUpdateFrequency * 1000000)
        )
      pure flowRt
    runFlowR flowRt' env action
