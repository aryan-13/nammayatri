{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Scheduler.ScheduleJob
  ( createJob,
    createJobByTime,
    createJobIn,
  )
where

import Data.Singletons
import Kernel.Prelude hiding (mask, throwIO)
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.Types

createJob ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  JobEntry e ->
  m (Id AnyJob)
createJob createJobFunc jobEntry = do
  now <- getCurrentTime
  createJobImpl createJobFunc now jobEntry

createJobIn ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  NominalDiffTime ->
  JobEntry e ->
  m (Id AnyJob)
createJobIn createJobFunc diff jobEntry = do
  now <- getCurrentTime
  when (diff < 0) $ throwError $ InternalError "job can only be scheduled for now or for future"
  let scheduledAt = addUTCTime diff now
  createJobImpl createJobFunc scheduledAt jobEntry

createJobByTime ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  UTCTime ->
  JobEntry e ->
  m (Id AnyJob)
createJobByTime createJobFunc scheduledAt jobEntry = do
  now <- getCurrentTime
  when (scheduledAt <= now) $
    throwError $
      InternalError
        "job can only be scheduled for the future\
        \ using createJobByTime, for scheduling for\
        \ now use createJobIn function instead"
  createJobImpl createJobFunc scheduledAt jobEntry

createJobImpl ::
  forall t (e :: t) m.
  (MonadTime m, MonadGuid m, MonadThrow m, Log m, SingI e, JobProcessor t, JobInfoProcessor (e :: t)) =>
  (AnyJob t -> m ()) ->
  UTCTime ->
  JobEntry e ->
  m (Id AnyJob)
createJobImpl createJobFunc scheduledAt JobEntry {..} = do
  when (maxErrors <= 0) $ throwError $ InternalError "maximum errors should be positive"
  now <- getCurrentTime
  id <- Id <$> generateGUIDText
  let job = makeJob id now
  createJobFunc $ AnyJob job
  pure id
  where
    makeJob id currentTime =
      Job
        { id = id,
          jobInfo = JobInfo (sing :: Sing e) jobData,
          scheduledAt = scheduledAt,
          maxErrors = maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = Pending
        }
