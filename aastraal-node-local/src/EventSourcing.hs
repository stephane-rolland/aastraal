module EventSourcing where

import qualified FileDataBase as FDB 

import Task
import TimeLog

data Event =
  Refresh
  | TimeLogged TimeLogs
  | TaskCreated TaskName TaskUuid TaskUuid -- last one is parents uuid
  | TaskSetDescription TaskUuid TaskDescription
  | TaskSetWhy TaskUuid TaskWhy
  | TaskSetStatus TaskUuid TaskStatus
  | TaskSetAssurance TaskUuid TaskAssurance
  | TaskSetCynefin TaskUuid TaskCynefin
  | TaskSetValue TaskUuid TaskValue
  | TaskSetEstimate TaskUuid TaskEstimate
  | TaskSetPerturbation TaskUuid TaskPerturbation
  | TaskSetParent TaskUuid TaskUuid
  deriving (Show,Read)

type Events = [Event]

dbEventStoreFileName :: String
dbEventStoreFileName = "/home/code/owncloud/wbs/aastraal/dbEventStore.txt"

store :: Event -> IO ()
store e = do
  events <- dbLoad
  let events' = e : events
  dbSave events'

dbLoad :: IO (Events)
dbLoad = do
  events <- FDB.dbLoad readEvent dbEventStoreFileName
  return events

dbSave :: Events -> IO ()
dbSave events = do
  FDB.dbSave writeEvent events dbEventStoreFileName
  return ()

readEvent :: String -> Event
readEvent s = read s :: Event

writeEvent :: Event -> String
writeEvent e = show e
