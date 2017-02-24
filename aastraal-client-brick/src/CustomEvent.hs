module CustomEvent where

import qualified Graphics.Vty as GraphicsVty
import qualified Notification as N
import qualified Data.Time.Clock as DTC

data CustomEvent = VtyEvent GraphicsVty.Event
                 | OnNewCliCommand
                 | OnNotification N.Notification
                 | OnTimeLogClock DTC.UTCTime
