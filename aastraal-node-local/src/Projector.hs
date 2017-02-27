{-# LANGUAGE TypeOperators #-}

module Projector where

import qualified EventSourcing as ES
import qualified FileDataBase as FDB 
import Task
import TimeLog
import qualified Notification as N
import qualified Data.List as DL
import qualified Data.Maybe as DM
import Control.Lens
import Control.Type.Operator -- so to write IO $ Maybe a instead of IO (Maybe a)


dbTasksFileName :: String
dbTasksFileName = "/home/code/owncloud/wbs/aastraal/dbTasks.txt"

dbTimeLogsFileName :: String
dbTimeLogsFileName = "/home/code/owncloud/wbs/aastraal/dbTimeLogs.txt"

taskTransformer :: TaskUuid -> (Task -> Task) -> Tasks -> Tasks
taskTransformer taskUuid f ts = newTask : ts''
  where
    (t,ts'') = taskWithUuid taskUuid ts 
    newTask = f t

update :: ES.Event -> IO $ N.Notifications
update (ES.Refresh) = do
  ts <- workWithTaskDb id
  tls <- workWithTimeLogDb id
  return $ N.UpdateTimeLogs tls : N.UpdateTasks ts : []
update (ES.TimeLogged tls) = do
  tls' <- workWithTimeLogDb $ transformTimeLogs tls
  putStrLn $ "debugging projector update = " ++ show tls'
  return $ N.UpdateTimeLogs tls' : []
  where
    transformTimeLogs ts' ts = ts' ++ ts
update (ES.TaskCreated taskName taskUuid taskUuidParent) = do
  ts <- workWithTaskDb transformTasks
  return $ N.UpdateTasks ts : []
  where
    transformTasks ts = newTask : ts
    newTask'' = set name taskName $ mkDefaultTask
    newTask' = set uuid taskUuid $ newTask''
    newTask = set parent taskUuidParent $ newTask'
update (ES.TaskSetDescription taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set description p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetWhy taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set why p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetStatus taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set status p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetAssurance taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set assurance p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetCynefin taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set cynefin p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetValue taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set value p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetEstimate taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set estimate p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetPerturbation taskUuid p) = do
  ts <- workWithTaskDb $ taskTransformer taskUuid $ set perturbation p
  return $ N.UpdateTasks ts : []
update (ES.TaskSetParent u u') = do
  putStrLn $ "receive set parent u = " ++ u ++ " u' = " ++ u' 
  ts <- workWithTaskDb $ taskTransformer u' $ set parent u
  return $ N.UpdateTasks ts : []

taskWithUuid :: TaskUuid -> Tasks -> (Task,Tasks)
taskWithUuid u ts = (task,ts')
  where
    maybeTask = DL.find (predicateUuid u) ts
    task = DM.fromJust maybeTask
    ts' = filter (not . (predicateUuid u)) ts
  
workWithTaskDb :: (Tasks -> Tasks) -> IO Tasks 
workWithTaskDb transformTasks = do
  tasks <- FDB.dbLoad readTask dbTasksFileName
  let tasks' = transformTasks tasks
  FDB.dbSave writeTask tasks' dbTasksFileName
  return tasks'

workWithTimeLogDb :: (TimeLogs -> TimeLogs) -> IO TimeLogs
workWithTimeLogDb transformTimeLogs = do
  timelogs <- FDB.dbLoad readTimeLog dbTimeLogsFileName
  let timelogs' = transformTimeLogs timelogs
  FDB.dbSave writeTimeLog timelogs' dbTimeLogsFileName
  return timelogs'
  
readTask :: String -> Task
readTask s = read s :: Task

writeTask :: Task -> String
writeTask t = show t

readTimeLog :: String -> TimeLog
readTimeLog s = read s :: TimeLog

writeTimeLog :: TimeLog -> String
writeTimeLog tl = show tl


fetchByUuid :: TaskUuid -> IO $ Maybe Task
fetchByUuid taskUuid = do
  tasks <- FDB.dbLoad readTask dbTasksFileName
  let maybeTask = DL.find (predicateUuid taskUuid) tasks
  return maybeTask

fetchByNameAndParentUuid :: TaskName -> TaskUuid -> IO (Maybe Task)
fetchByNameAndParentUuid n pu = do
  tasks <- FDB.dbLoad readTask dbTasksFileName
  let maybeTask = DL.find (predicateNameAndParentUuid n pu) tasks
  return maybeTask
 
predicateUuid :: TaskUuid -> Task -> Bool 
predicateUuid tuuid task = tuuid == view uuid task

predicateNameAndParentUuid :: TaskName -> TaskUuid -> Task -> Bool
predicateNameAndParentUuid n pu t = isNameMatch && isParentUuidMatch
  where
    isParentUuidMatch = pu == (view parent t) 
    isNameMatch = tName == n
      where
        nbChar = length n
        tName = take nbChar $ view name t
