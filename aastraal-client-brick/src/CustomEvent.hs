module CustomEvent where

import qualified Graphics.Vty as GraphicsVty
import qualified Notification as N


data CustomEvent = VtyEvent GraphicsVty.Event
                 | OnNewCliCommand
                 | OnNotification N.Notification
