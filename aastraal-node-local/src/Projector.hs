module Projector where

import qualified EventSourcing as ES
import qualified FileDataBase as FDB 
import Task
import qualified Notification as N
import qualified Data.List as DL
import qualified Data.Maybe as DM
import Control.Lens

dbTasksFileName :: String
dbTasksFileName = "/home/code/owncloud/wbs/aastraal/dbTasks.txt"

taskTransformer :: TaskUuid -> (Task -> Task) -> Tasks -> Tasks
taskTransformer taskUuid f ts = newTask : ts''
  where
    (t,ts'') = taskWithUuid taskUuid ts 
    newTask = f t

update :: ES.Event -> IO (N.Notification)
update (ES.Refresh) = do
  ts <- workWithDb id
  return $ N.UpdateTasks ts
update (ES.TaskCreated taskName taskUuid taskUuidParent) = do
  ts <- workWithDb transformTasks
  return $ N.UpdateTasks ts
  where
    transformTasks ts = newTask : ts
    newTask'' = set name taskName $ mkDefaultTask
    newTask' = set uuid taskUuid $ newTask''
    newTask = set parent taskUuidParent $ newTask'
update (ES.TaskSetDescription taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set description p
  return $ N.UpdateTasks ts
update (ES.TaskSetWhy taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set why p
  return $ N.UpdateTasks ts
update (ES.TaskSetStatus taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set status p
  return $ N.UpdateTasks ts
update (ES.TaskSetAssurance taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set assurance p
  return $ N.UpdateTasks ts
update (ES.TaskSetCynefin taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set cynefin p
  return $ N.UpdateTasks ts
update (ES.TaskSetValue taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set value p
  return $ N.UpdateTasks ts
update (ES.TaskSetEstimate taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set estimate p
  return $ N.UpdateTasks ts
update (ES.TaskSetPerturbation taskUuid p) = do
  ts <- workWithDb $ taskTransformer taskUuid $ set perturbation p
  return $ N.UpdateTasks ts
update (ES.TaskSetParent u u') = do
  putStrLn $ "receive set parent u = " ++ u ++ " u' = " ++ u' 
  ts <- workWithDb $ taskTransformer u' $ set parent u
  return $ N.UpdateTasks ts

taskWithUuid :: TaskUuid -> Tasks -> (Task,Tasks)
taskWithUuid u ts = (task,ts')
  where
    maybeTask = DL.find (predicateUuid u) ts
    task = DM.fromJust maybeTask
    ts' = filter (not . (predicateUuid u)) ts
  
workWithDb :: (Tasks -> Tasks) -> IO (Tasks) 
workWithDb transformTasks = do
  tasks <- FDB.dbLoad readTask dbTasksFileName
  let tasks' = transformTasks tasks
  FDB.dbSave writeTask tasks' dbTasksFileName
  return tasks'
  
readTask :: String -> Task
readTask s = read s :: Task

writeTask :: Task -> String
writeTask t = show t

fetchByUuid :: TaskUuid -> IO (Maybe Task)
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
