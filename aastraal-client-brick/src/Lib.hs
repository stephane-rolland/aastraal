{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import AppState

import qualified Graphics.Vty as GraphicsVty
import CustomEvent


import qualified Brick.Main as BrickMain
import qualified Brick.Types as BrickTypes
 
-- import qualified BrickTypes.Widget as BrickWidget
-- import qualified Brick.Widgets.Center as BrickWidgetsCenter
import qualified Brick.Widgets.Edit as BrickWidgetsEdit
import qualified Brick.AttrMap as BrickAttrMap

import Brick.Widgets.Core
 ( (<+>)
 , (<=>)
 --, hLimit
 , vLimit
 , str
 , padTop
 )

import Brick.Markup
import Data.Text.Markup  -- for @@ using color
import Data.Text (pack)

import Brick.Util (on,fg)
import Control.Lens
import KeyHandler

import qualified Control.Monad as ControlMonad
import qualified Control.Concurrent as ControlConcurrent
import qualified Data.Default as DataDefault 

import qualified Notification as N
import qualified NotificationHandler as NH

import Task 
import TimeLog
import qualified System.IO as SIO
import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as O
import qualified Data.Time.Clock as DTC
import qualified Control.Concurrent.Timer as CCT
import qualified Control.Concurrent.Suspend.Lifted as CCSL

type CursorLocationName = BrickTypes.CursorLocation Name

drawUI :: St -> [BrickTypes.Widget Name]
drawUI st = [cli]
    where
        tasksTxt = str $ getTasksText st
        e1 =  BrickWidgetsEdit.renderEditor False (st ^. cliEditor)
        --ui = BrickWidgetsCenter.center $ (str "Log Task: " <+> (hLimit 30 $ vLimit 5 $ e1 )) <=>
        --                str " " <=>
        --                str "this task will be logged every minute"
        errorTxt = markup ((pack $ view lastError st) @@ fg GraphicsVty.red)
        cli = tasksTxt <=> (padTop BrickTypes.Max $ str "Enter command: " <+> (vLimit 1 $ e1)) <=> errorTxt

appCursor :: St -> [CursorLocationName] -> Maybe CursorLocationName
appCursor st names = locationName
  where
    locationName :: Maybe CursorLocationName
    locationName = BrickMain.showCursorNamed (st ^. currentEditor) names

appEvent :: St -> CustomEvent -> EventMName (BrickTypes.Next St)
appEvent st (VtyEvent ev) = do
  nextEvent <-  case ev of
        GraphicsVty.EvKey keyCode [] -> KeyHandler.onKeyEvent keyCode st ev
        _ -> BrickMain.continue st 
        -- _ -> BrickMain.continue =<< BrickTypes.handleEventLensed st (edit1) BrickWidgetsEdit.handleEditorEvent ev
  return (nextEvent)
appEvent st OnNewCliCommand = do
  handleOnNewCliCommand st
  
appEvent st (OnNotification n) = do
  let st' = NH.updateState st n
  BrickMain.continue st'
appEvent st (OnTimeLogClock datetime) = do
  let st' = handleTimeLogClock st datetime
  BrickMain.continue st'


theMap :: BrickAttrMap.AttrMap
theMap = BrickAttrMap.attrMap GraphicsVty.defAttr
    [ (BrickWidgetsEdit.editAttr, GraphicsVty.white `on` GraphicsVty.brightBlack)
    ]

theApp :: BrickMain.App St CustomEvent Name
theApp =
    BrickMain.App { BrickMain.appDraw = drawUI
          , BrickMain.appChooseCursor = appCursor
          , BrickMain.appHandleEvent = appEvent
          , BrickMain.appStartEvent = return
          , BrickMain.appAttrMap = const theMap
          --, BrickMain.appLiftVtyEvent = id
          , BrickMain.appLiftVtyEvent = VtyEvent
          }

main :: IO ()
main = do

  handle <- NH.connect
  let startState = set socketHandle handle initialState
  
  chan <- ControlConcurrent.newChan

  launchTimer chan
  threadHandleNotifications (DM.fromJust handle) chan
  threadHandleNewCliCommands chan
 
  st <- BrickMain.customMain (GraphicsVty.mkVty DataDefault.def) chan theApp startState

  let listCommands = st ^. commands

  putStrLn "application stopped\n"
  putStrLn "current commands: "
  putStrLn $ show $ listCommands

threadHandleNewCliCommands :: ControlConcurrent.Chan CustomEvent -> IO ()
threadHandleNewCliCommands chan = do
  _ <- ControlConcurrent.forkIO $ ControlMonad.forever $ do
    ControlConcurrent.writeChan chan OnNewCliCommand
    ControlConcurrent.threadDelay 200000
  return ()

threadHandleNotifications :: SIO.Handle -> ControlConcurrent.Chan CustomEvent -> IO ()
threadHandleNotifications handle chan = do
  _ <- ControlConcurrent.forkIO $ do
    NH.treatEachMsg handle $ (onNotification chan)
  return ()

onNotification :: ControlConcurrent.Chan CustomEvent -> N.Notification -> IO ()
onNotification chan n = do
  ControlConcurrent.writeChan chan (OnNotification n)
  return ()

launchTimer :: ControlConcurrent.Chan CustomEvent -> IO ()
launchTimer chan = do
  let delay = CCSL.mDelay 1
  _ <- CCT.repeatedTimer (onTimeLogClockComputation chan) delay
  return ()

onTimeLogClockComputation :: ControlConcurrent.Chan CustomEvent -> IO ()
onTimeLogClockComputation chan = do
  currentTime <- DTC.getCurrentTime
  ControlConcurrent.writeChan chan (OnTimeLogClock currentTime)

        
getTasksText :: St -> String
getTasksText st = concat $ map display sorted
  where
    display = displayTaskAsText st 
    ts = view tasks st
    selected = filter (isCurrentTaskOrDirectChild $ st) ts
    sorted = DL.sortBy comparer selected

    comparer :: Task -> Task -> O.Ordering
    comparer t t' = if isFirstParent
                    then O.LT
                    else if isSecondParent
                         then O.GT
                         else O.compare (view name t) (view name t')
      where
        isFirstParent = pu' == u
        isSecondParent = pu == u'
        u   = view uuid t
        pu  = view parent t
        u'  = view uuid t'
        pu' = view parent t'

displayTaskAsText :: St -> Task -> String
displayTaskAsText st t@(Task n _u d _p w s a c v e perturb) =
  if isToBeDisplayed
  then textToDisplay
  else "do not display" ++ n 

  where
    textToDisplay = padding ++ dashField ++ n ++ timelogged ++ descriptionField ++ whyField ++ newline ++ details

    timelogged = " {" ++ (show $ getTimeLogged st t) ++ ":00} "
    depth = getDepth st t 
    padding = concat $ replicate depth "           "
    newline = "\n"

    isToBeDisplayed = isCurrentTaskOrDirectChild st t
    isDetailed = view isShowDetails st
    dashField = case s of
                  ToDo -> "  -  "
                  Urgent -> " !!! "
                  Doing -> "  o  "
                  Done -> "  x  "
                  Delayed -> " ... "
                  Cancelled -> " xxx "
                  Problem -> " /!\\ "
                  
    descriptionField = if (not.null) d then "    (" ++ d ++ ")" else ""  
    whyField = if (not.null) w then "   why = " ++ w  else "" 
    details = if not isDetailed then "" else padding ++ furtherDetails ++ newline
    furtherDetails = "     >>> " ++
      "[efficiency = " ++ (show $ getRatio a c v e perturb) ++
      " assurance = " ++ (show a) ++
      " cynefin = " ++ (show c) ++
      " value = " ++ (show v) ++
      " estimate = " ++ (show e) ++
      " perturbation = " ++ (show perturb) ++ "]" 

getRatio :: TaskAssurance -> TaskCynefin -> TaskValue -> TaskEstimate -> TaskPerturbation -> Int
getRatio a c v e p = numerator `quot` denominator
  where
    complexity = getComplexity c
    numerator = 100 * ( if v == 0 then 1 else v) * a * complexity * complexity
    denominator = e * p

getComplexity :: TaskCynefin -> Int
getComplexity Unknown = 8 
getComplexity Chaos = 5
getComplexity Complex = 3
getComplexity Complicated = 2
getComplexity Obvious = 1


getDepth :: St -> Task -> Int
getDepth st t = getDepthRecursive ts t selectedUuid
  where
    ts = view tasks st
    selectedUuid = view uuidCurrentTask st

getDepthRecursive :: Tasks -> Task -> TaskUuid -> Int
getDepthRecursive _ts task selectedU | (view uuid task) == selectedU = 0
getDepthRecursive _ts task _selectedU | (view parent task) == "" = 0
getDepthRecursive _ _ _ = 1

isCurrentTaskOrDirectChild :: St -> Task -> Bool
isCurrentTaskOrDirectChild st t = isCurrentTask || isChildTask
  where
    uuidParent = view uuidCurrentTask st
    isCurrentTask = uuidParent == (view uuid t)
    isChildTask = uuidParent == (view parent t)


handleTimeLogClock :: St -> DTC.UTCTime -> St
handleTimeLogClock st dt = st''
  where
    tls = view timeLogsToSend st
    txt = view timeLogComment st
    currentTaskLogged = view uuidCurrentTaskLogged st

    newTimeLog = case currentTaskLogged of
                   Just u -> Just $ TimeLog u dt txt
                   _ -> Nothing 

    tls' = case newTimeLog of
             Just tl -> tl : tls
             _ -> tls
    
    st' = set timeLogsToSend tls' st
    st'' = set timeLogComment "" st'
  
getTimeLogged :: St -> Task -> Int
getTimeLogged st t = selfTimeLogged + childTimeLogged
  where
    selfTimeLogged = length $ getTimeLogs st t
    childTimeLogged = sum $ map (getTimeLogged st) cs
    cs = getChildren st t


getChildren :: St -> Task -> Tasks
getChildren st t = cs
  where
    ts = view tasks st
    cs = filter (predicate t) ts
    predicate :: Task -> Task -> Bool
    predicate task t' = (view uuid task) == (view parent t')


getTimeLogs :: St -> Task -> TimeLogs
getTimeLogs st t = tls'
  where
    tls = view timeLogs st
    tls' = filter (predicate t) tls
    predicate :: Task -> TimeLog -> Bool
    predicate task tl = (view uuid task) == (view relatedTaskUuid tl)























