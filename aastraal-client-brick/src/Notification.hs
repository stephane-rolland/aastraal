module Notification where

import qualified Task as T

type Notifications = [Notification]

data Notification = UpdateTasks T.Tasks deriving (Show,Read) 

