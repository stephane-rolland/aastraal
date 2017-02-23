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

initialState :: St
initialState =
    St firstEditor
       (BrickWidgetsEdit.editor firstEditor (str . unlines) Nothing "refresh")
       noInitialCommands
       noNetworkConnectionHandle
       noInitialTasks
       noInitialTaskSelected
       initialShowDetails
       
data St =
    St { _currentEditor :: Name
       , _cliEditor :: EditorCtrl
       , _commands :: [StringCommand]
       , _socketHandle :: Maybe SIO.Handle
       , _tasks :: T.Tasks
       , _uuidCurrentTask :: String
       , _isShowDetails :: Bool
       }

makeLenses ''St
