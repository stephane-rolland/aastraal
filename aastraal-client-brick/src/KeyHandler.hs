{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards#-}
module KeyHandler where

import qualified Brick.Main as BrickMain
import qualified Brick.Types as BrickTypes
import qualified Graphics.Vty as GraphicsVty

import qualified Brick.Widgets.Edit as BrickWidgetsEdit 

import AppState
import Control.Lens

import Data.Text.Zipper

import qualified Control.Monad.IO.Class as IOClass
import Command

import IoNetwork

import qualified Data.Maybe as DM 
import qualified System.IO as SIO

import Task 
import TimeLog

type Key = GraphicsVty.Key
type EventMName = BrickTypes.EventM Name

type VtyEvent = GraphicsVty.Event


onKeyEvent :: Key -> St -> VtyEvent -> EventMName (BrickTypes.Next St)
onKeyEvent keycode st ev = do
  nextEvent <- case keycode of
        GraphicsVty.KEsc -> BrickMain.halt st
        GraphicsVty.KEnter -> handleKeyEnter st
        _ -> BrickMain.continue =<< BrickTypes.handleEventLensed st (cliEditor) BrickWidgetsEdit.handleEditorEvent ev
  --      GraphicsVty.KEnter -> SuspendAndResume IO (st)
  return (nextEvent)

enterNewCLICommand :: St -> [String] -> St
enterNewCLICommand st cs' = st'
  where
    cs = view commands st
    st' = set commands cs'' st
    cs''= (head cs') : cs   -- be careful here, it only takes the first command line it seems and discards the rest of the edit lines

handleKeyEnter :: St -> EventMName (BrickTypes.Next St)
handleKeyEnter st = do
  let st'' = eraseCommandLine' st'
  BrickMain.continue st''
  where
    editor = st ^. cliEditor          
    editContents = BrickWidgetsEdit.getEditContents $ editor
    st' = enterNewCLICommand st editContents

resetEdit :: St -> EditorCtrl
resetEdit st = BrickWidgetsEdit.applyEdit transformer $ st ^. cliEditor  
  where
    transformer :: TextZipper String -> TextZipper String
    transformer _ = stringZipper [] Nothing

eraseCommandLine :: St -> St
eraseCommandLine st = st'
  where
    e'= resetEdit st
    st' = set cliEditor e' st 

eraseCommandLine' :: St -> St
eraseCommandLine' st = st & cliEditor .~ (resetEdit st)


handleUserActivity :: St -> IO (St) 
handleUserActivity st = do
  newState <- if not hasTimeLogs
              then return st
              else do
                  IOClass.liftIO $ sendTimeLogs handle tls 
                  return $ set timeLogsToSend [] st 
  return newState

  where
    tls = view timeLogsToSend st
    hasTimeLogs = not.null $ tls  
    handle = DM.fromJust $ view socketHandle st 


handleOnNewCliCommand :: St -> EventMName (BrickTypes.Next St)
handleOnNewCliCommand st = do
  newState <- if not hasCommands
        then return st
        else do
            st' <- IOClass.liftIO $ parseAndEvaluateCommand handle c st
            let st'' = set commands (drop 1 cs) st'
            st''' <- IOClass.liftIO $  handleUserActivity st''    -- what to do when there is user activity ( send time logs for example)
            return st'''

  BrickMain.continue newState
  where
    hasCommands = not . null $ st ^. commands 
    cmds    = st ^. commands
    (c:cs) = cmds
    handle = DM.fromJust $ st ^. socketHandle
  
parseAndEvaluateCommand :: SIO.Handle -> String -> St -> IO (St)
parseAndEvaluateCommand handle cmd st = do
  commandToSend <- parse cmd
  st' <- case commandToSend of
    Right c -> do
      st' <- evaluateCommand c handle st
      return st'
    Left msg -> do
      putStrLn msg
      return st
  return st'


evaluateCommand :: Command -> SIO.Handle -> St -> IO (St)
evaluateCommand (AppShowDetails b) _ st = do
  let st' = set isShowDetails b st
  return st'
evaluateCommand (TimeLogStart cmt) _ st = do
  let uuidSelected = view uuidCurrentTask st
  let st' = set uuidCurrentTaskLogged (Just uuidSelected) st
  let st''= set timeLogComment cmt st'
  return st''
evaluateCommand (TimeLogStop) _ st = do
  let st' = set uuidCurrentTaskLogged Nothing st
  return st'
evaluateCommand (TimeLogComment cmt) _ st = do
  let st' = set timeLogComment cmt st
  return st'
  
evaluateCommand (TaskSelect taskName) _ st = do
  let uuidSelected = getUuidSelected taskName $ view tasks st
  let st' = set uuidCurrentTask uuidSelected st
  return st'
evaluateCommand (TaskCreate n u _up) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskCreate n u u') 
  return st
evaluateCommand (TaskSetDescription _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetDescription u' d)
  return st
evaluateCommand (TaskSetWhy _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetWhy u' d)
  return st
evaluateCommand (TaskSetStatus _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetStatus u' d)
  return st
evaluateCommand (TaskSetAssurance _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetAssurance u' d)
  return st
evaluateCommand (TaskSetCynefin _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetCynefin u' d)
  return st
evaluateCommand (TaskSetValue _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetValue u' d)
  return st
evaluateCommand (TaskSetEstimate _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetEstimate u' d)
  return st
evaluateCommand (TaskSetPerturbation _ d) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetPerturbation u' d)
  return st
evaluateCommand (TaskSetParent _ pn cn) h st = do
  let u' = view uuidCurrentTask st
  sendOverNetwork h (TaskSetParent u' pn cn)
  return st
evaluateCommand c h st = do
  sendOverNetwork h c
  return st

getUuidSelected :: TaskName -> Tasks -> TaskUuid
getUuidSelected n ts = uuidFound
  where
    uuidFound = if not.null $ selected then view uuid $ head selected else ""
    selected = filter (predicate n) ts
    predicate nm t = n == startTaskName
      where
        taskName = view name t
        lengthInput = length nm
        startTaskName = take lengthInput taskName


sendTimeLogs :: SIO.Handle -> TimeLogs -> IO ()
sendTimeLogs h tls = sendOverNetwork h (TimeLogged tls)
  



-- eraseCommandLine :: St -> EventMName (BrickTypes.Next St)
-- eraseCommandLine st = do
--  let e = resetEdit st
--  IOClass.liftIO $ putStrLn $ show e
--  BrickMain.continue st 

--emptyEditorEvent :: St -> EventMName (BrickTypes.Next St)
--emptyEditorEvent st = do
--   let test = edit1 & BrickWidgetsEdit.editContentsL .~ stringZipper [] Nothing
--   BrickMain.continue $ st











