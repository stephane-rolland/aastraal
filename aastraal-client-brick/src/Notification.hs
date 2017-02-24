module Notification where

import qualified Task as T
import qualified TimeLog as TL

type Notifications = [Notification]

data Notification =   UpdateTasks T.Tasks
                    | UpdateTimeLogs TL.TimeLogs 
                    deriving (Show,Read) 

