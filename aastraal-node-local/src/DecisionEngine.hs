module DecisionEngine where

import Command
import qualified EventSourcing as ES
import qualified Projector as P

import Task
import Control.Lens

type DecisionEngine = Command -> SubscriberNotification -> IO ()
type SubscriberNotification = String -> IO ()


processEvent :: ES.Event -> SubscriberNotification -> IO ()
processEvent event notifier = do
  ES.store event
  notification <- P.update event
  notifier $ show notification

process :: Command -> SubscriberNotification -> IO ()
process AppRefresh notifier = processEvent ES.Refresh notifier
process (TaskCreate taskName taskUuid taskUuidParent) notifier = do
  let event = ES.TaskCreated taskName taskUuid taskUuidParent
  processEvent event notifier
process (TaskSetDescription u w) n = processTaskUuid u n (\x -> ES.TaskSetDescription x w)
process (TaskSetWhy u w) n = processTaskUuid u n (\x -> ES.TaskSetWhy x w)
process (TaskSetStatus u w) n = processTaskUuid u n (\x -> ES.TaskSetStatus x w)
process (TaskSetAssurance u w) n = processTaskUuid u n (\x -> ES.TaskSetAssurance x w)
process (TaskSetCynefin u w) n = processTaskUuid u n (\x -> ES.TaskSetCynefin x w)
process (TaskSetValue u w) n = processTaskUuid u n (\x -> ES.TaskSetValue x w)
process (TaskSetEstimate u w) n = processTaskUuid u n (\x -> ES.TaskSetEstimate x w)
process (TaskSetPerturbation u w) n = processTaskUuid u n (\x -> ES.TaskSetPerturbation x w)
process (TaskSetParent u pn cn) n = processSetParent u pn cn n
process c _ = error $ "this command cannot be treated by the server: " ++ (show c)


fetchTask :: TaskUuid -> IO (Maybe Task)
fetchTask taskUuid = P.fetchByUuid taskUuid

fetchChildTask :: TaskUuid -> TaskName -> IO (Maybe Task)
fetchChildTask pu cn = P.fetchByNameAndParentUuid cn pu

processTaskUuid :: TaskUuid -> SubscriberNotification -> (TaskUuid -> ES.Event) -> IO () 
processTaskUuid u notifier f = do
  maybeTask <- fetchTask u
  let maybeEvent = case maybeTask of
                     Just _t -> Just $ f u
                     _ -> Nothing
  case maybeEvent of
    Just e -> processEvent e notifier
    _ -> return ()

processSetParent :: TaskUuid -> TaskName -> TaskName -> SubscriberNotification -> IO()
processSetParent u pn cn notifier = do
  maybeTaskParent <- fetchChildTask u pn
  maybeTaskChild <- fetchChildTask u cn
  putStrLn $ "parent: " ++ (show maybeTaskParent) ++ "child: " ++ (show maybeTaskChild)
  let maybeEvent = case (maybeTaskParent,maybeTaskChild) of
                     (Just pt, Just ct) -> Just $ ES.TaskSetParent (view uuid pt) (view uuid ct)
                     _ -> Nothing
  case maybeEvent of
    Just e -> processEvent e notifier
    _ -> return ()
  

  -- in case of create, we always create a new task even though one with same label exists
  -- improvement would be to warn that one already exists
  -- and have a unification function to merge two existing tasks
--  task <- fetchState uuid
--  let event = decider state cmd 
--      where
--        decider = undefined
--        cmd = undefined

-- fetchState :: TaskUuid -> IO (Maybe T.Task)
-- fetchState uuid = do
--  putStrLn $ "Plan to load db"
--  tasks <- DB.dbLoad
--  putStrLn $ "db should be loaded"
--  putStrLn $ show $ tasks
--  let task = head tasks
--  return $ Just task



-- CQRS + Event Sourcing
-- recevies comands
-- from commands, decide to create event
-- from events, store them
-- from events, update read-db
-- from update read-db, send notification to clients subscribed to projections

  
