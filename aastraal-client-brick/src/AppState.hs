{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-} -- forall t. 

module AppState where

import Control.Lens
import qualified Brick.Widgets.Edit as BrickWidgetsEdit
import qualified Brick.Types as BrickTypes

import Brick.Widgets.Core ( str )

import qualified System.IO as SIO
import qualified Task as T
import qualified TimeLog as TL

type Name = String
type EditorCtrl = BrickWidgetsEdit.Editor String Name
type WidgetName = BrickTypes.Widget Name

type StringCommand = String

firstEditor :: Name
firstEditor = "edit1"

noNetworkConnectionHandle :: Maybe SIO.Handle
noNetworkConnectionHandle = Nothing
noInitialCommands :: [a]
noInitialCommands = []
noInitialTasks :: forall t. [t]
noInitialTasks = []
noInitialTaskSelected :: String
noInitialTaskSelected = ""
initialShowDetails :: Bool 
initialShowDetails = False
noCurrentTaskLogged :: Maybe T.TaskUuid
noCurrentTaskLogged = Nothing
noTimeLogs :: [a]
noTimeLogs = []
noTimeLogComment :: String
noTimeLogComment = ""

initialState :: St
initialState =
    St firstEditor
       (BrickWidgetsEdit.editor firstEditor (str . unlines) Nothing "refresh")
       noInitialCommands
       noNetworkConnectionHandle
       noInitialTasks
       noInitialTaskSelected
       initialShowDetails
       noCurrentTaskLogged
       noTimeLogs   -- no time logs to send
       noTimeLogComment
       noTimeLogs  -- no time logs about tasks
       
data St =
    St { _currentEditor :: Name
       , _cliEditor :: EditorCtrl
       , _commands :: [StringCommand]
       , _socketHandle :: Maybe SIO.Handle
       , _tasks :: T.Tasks
       , _uuidCurrentTask :: T.TaskUuid
       , _isShowDetails :: Bool
       , _uuidCurrentTaskLogged :: Maybe T.TaskUuid
       , _timeLogsToSend :: TL.TimeLogs
       , _timeLogComment :: String
       , _timeLogs :: TL.TimeLogs
       }

makeLenses ''St
