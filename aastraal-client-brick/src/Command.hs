{-# LANGUAGE ScopedTypeVariables #-}
module Command where

import qualified Data.UUID.V1 as DataUuidV1
import qualified Data.UUID as DataUuid
import Data.Maybe
import qualified Data.List as DL

import Task
import TimeLog

type Parser = [String] -> IO (Either String Command)  

mkTaskCreate :: TaskName -> IO (Command)
mkTaskCreate n = do
    u <- DataUuidV1.nextUUID
    let justUuid = fromJust u 
    let string = DataUuid.toString justUuid
    return $ TaskCreate n string ""

data Command =   AppRefresh
               | AppShowDetails Bool
               | TimeLogStart String
               | TimeLogStop
               | TimeLogComment String
               | TaskCreate TaskName TaskUuid TaskUuid   -- the last one it hte parent uuid
               | TaskSelectParent 
               | TaskSelect TaskName
               | TaskSetDescription TaskUuid TaskDescription
               | TaskSetWhy TaskUuid TaskWhy
               | TaskSetStatus TaskUuid TaskStatus
               | TaskSetAssurance TaskUuid TaskAssurance
               | TaskSetCynefin TaskUuid TaskCynefin
               | TaskSetValue TaskUuid TaskValue
               | TaskSetEstimate TaskUuid TaskEstimate
               | TaskSetPerturbation TaskUuid TaskPerturbation
               | TaskSetParent TaskUuid TaskName TaskName  -- currentUuid parentName childName
               | TimeLogged TimeLogs 
             deriving (Show, Read)

parse :: String -> IO (Either String Command)
parse cmdLine = let args = words cmdLine in parseElems args 

parseElems :: Parser
parseElems ("app-show-details" : args) = parseAppShowDetails args
parseElems ("show" : _) = return $ Right $ AppShowDetails True
parseElems ("hide" : _) = return $ Right $ AppShowDetails False

parseElems ("refresh" : _) = return $ Right AppRefresh
parseElems ("app-refresh" : _) = return $ Right AppRefresh

parseElems ("timelog-start" : args) = parseTimeLogStart args
parseElems ("ss" : args) = parseTimeLogStart args

parseElems ("timelog-stop" : _) = return $ Right TimeLogStop
parseElems ("st" : _) = return $ Right TimeLogStop

parseElems ("timelog-comment" : args) = parseTimeLogComment args
parseElems ("cmt" : args) = parseTimeLogComment args


parseElems ("task-create" : args) = parseTaskCreate args
parseElems ("touch" : args) = parseTaskCreate args

parseElems ("task-select-parent" : _) = parseTaskSelectParent []
parseElems ("cd" : ".." : _) = parseTaskSelectParent []

parseElems ("task-select" : args) = parseTaskSelect args
parseElems ("cd" : args) = parseTaskSelect args

parseElems ("task-set-description" : args) = parseTaskSetDescription args
parseElems ("desc" : args) = parseTaskSetDescription args

parseElems ("task-set-why" : args) = parseTaskSetWhy args
parseElems ("why" : args) = parseTaskSetWhy args

parseElems ("task-set-status" : args) = parseTaskSetStatus args
parseElems ("status" : args) = parseTaskSetStatus args
parseElems ("todo" : _) = return $ Right $ TaskSetStatus "" ToDo
parseElems ("urge" : _) = return $ Right $ TaskSetStatus "" Urgent
parseElems ("done" : _) = return $ Right $ TaskSetStatus "" Done
parseElems ("doing" : _) = return $ Right $ TaskSetStatus "" Doing
parseElems ("delay" : _) = return $ Right $ TaskSetStatus "" Delayed
parseElems ("cancel" : _) = return $ Right $ TaskSetStatus "" Cancelled
parseElems ("pblm" : _) = return $ Right $ TaskSetStatus "" Problem 

parseElems ("task-set-assurance" : args) = parseTaskSetAssurance args
parseElems ("assu" : args) = parseTaskSetAssurance args

parseElems ("task-set-complexity" : args) = parseTaskSetCynefin args
parseElems ("obvious" : _) = return $ Right $ TaskSetCynefin "" Obvious
parseElems ("complicated" : _) = return $ Right $ TaskSetCynefin "" Complicated
parseElems ("complex" : _) = return $ Right $ TaskSetCynefin "" Complex
parseElems ("chaos" : _) = return $ Right $ TaskSetCynefin "" Chaos
parseElems ("dunno" : _) = return $ Right $ TaskSetCynefin "" Unknown

parseElems ("task-set-value" : args) = parseTaskSetValue args
parseElems ("val" : args) = parseTaskSetValue args

parseElems ("task-set-estmate" : args) = parseTaskSetEstimate args
parseElems ("est" : args) = parseTaskSetEstimate args

parseElems ("task-set-perturbation" : args) = parseTaskSetPerturbation args
parseElems ("pert" : args) = parseTaskSetPerturbation args

parseElems ("task-set-parent" : args) = parseTaskSetParent args
parseElems ("mv" : args) = parseTaskSetParent args
 
parseElems as                    = parseError $ "in command: " ++ DL.intercalate " " as 


parseError :: String -> IO (Either String Command)
parseError s = return $ Left $ "Could not parse Command! " ++ s

parseAppShowDetails :: Parser
parseAppShowDetails (arg:_) = return $ Right $ AppShowDetails (read arg :: Bool) 
parseAppShowDetails as = parseError $ "in task-create: " ++ DL.intercalate " " as

parseTimeLogStart :: Parser
parseTimeLogStart as@(_:_) = return $ Right $ TimeLogStart $ DL.intercalate " " as
parseTimeLogStart _ = return $ Right $ TimeLogStart ""

parseTimeLogComment :: Parser
parseTimeLogComment as@(_:_) = return $ Right $ TimeLogComment $ DL.intercalate " " as
parseTimeLogComment as = parseError $ "in timelog-comment " ++ DL.intercalate " " as


parseTaskCreate :: Parser
parseTaskCreate as@(_:_)      = Right <$> mkTaskCreate (DL.intercalate " " as) 
parseTaskCreate as            = parseError $ "in task-create: " ++ DL.intercalate " " as

parseTaskSelectParent :: Parser
parseTaskSelectParent _ = return $ Right $ TaskSelectParent

parseTaskSelect :: Parser
parseTaskSelect as@(_:_) = return $ Right $ TaskSelect $ DL.intercalate " " as
parseTaskSelect as            = parseError $ "in task-select: " ++ DL.intercalate " " as

parseTaskSetDescription :: Parser
parseTaskSetDescription as@(_:_) = return $ Right $ TaskSetDescription "" $ DL.intercalate " " as
parseTaskSetDescription as = parseError $ "in task-set-description" ++ DL.intercalate " " as

parseTaskSetWhy :: Parser
parseTaskSetWhy as@(_:_) = return $ Right $ TaskSetWhy "" $ DL.intercalate " " as
parseTaskSetWhy as = parseError $ "in task-set-why" ++ DL.intercalate " " as

parseTaskSetStatus :: Parser
parseTaskSetStatus (arg:_) = return $ Right $ TaskSetStatus "" $ (read arg :: TaskStatus)
parseTaskSetStatus as = parseError $ "in task-set-status" ++ DL.intercalate " " as

parseTaskSetAssurance :: Parser
parseTaskSetAssurance (arg:_) = return $ Right $ TaskSetAssurance "" $ (read arg :: TaskAssurance) 
parseTaskSetAssurance as = parseError $ "in task-set-assurance" ++ DL.intercalate " " as

parseTaskSetCynefin :: Parser
parseTaskSetCynefin (arg:_) = return $ Right $ TaskSetCynefin "" $ (read arg :: TaskCynefin)
parseTaskSetCynefin as = parseError $ "in task-set-cynefin" ++ DL.intercalate " " as

parseTaskSetValue :: Parser
parseTaskSetValue (arg:_) = return $ Right $ TaskSetValue "" $ (read arg :: TaskValue)
parseTaskSetValue as = parseError $ "in task-set-value" ++ DL.intercalate " " as

parseTaskSetEstimate :: Parser
parseTaskSetEstimate (arg:_) = return $ Right $ TaskSetEstimate "" $ (read arg :: TaskEstimate)
parseTaskSetEstimate as = parseError $ "in task-set-estimate" ++ DL.intercalate " " as

parseTaskSetPerturbation :: Parser
parseTaskSetPerturbation (arg:_) = return $ Right $ TaskSetPerturbation "" $ (read arg :: TaskPerturbation)
parseTaskSetPerturbation as = parseError $ "in task-set-perturbation" ++ DL.intercalate " " as

parseTaskSetParent :: Parser
parseTaskSetParent as@(_:_:_) = parseParentArgs as
parseTaskSetParent as = parseError $ "in task-set-perturbation" ++ DL.intercalate " " as

parseParentArgs :: Parser
parseParentArgs as = do
  return $ Right $ TaskSetParent "" motherCleared child
  where
    mother  = DL.intercalate " " $ DL.dropWhile (not.predicate) as
    motherCleared = case mother of
                      ('.' : '/' : path) -> path
                      entire -> entire
    child = DL.intercalate " " $ DL.takeWhile (not.predicate) as
    predicate :: String -> Bool
    predicate ('.' : '/' : _) = True
    predicate _ = False
    
