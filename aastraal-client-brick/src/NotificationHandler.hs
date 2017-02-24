module NotificationHandler where

import qualified IoNetwork as IN
import qualified System.IO as SIO
import qualified Notification as N
import qualified Ack as A 
import qualified Text.Read as TR
import qualified Data.Maybe as DM
import Control.Lens
import AppState
import qualified Control.Monad as CM

treatEachMsg :: SIO.Handle -> (N.Notification -> IO ()) -> IO ()
treatEachMsg handle onNotification = do
    IN.receiveOverNetwork (treatMessage onNotification) handle 

data Parsed = Ack A.Ack
              | Notif N.Notifications

treatMessage :: (N.Notification -> IO ()) -> String -> IO ()
treatMessage onNotification s = do
  parsed <- parse s 
  case parsed of
    Ack _ -> return ()    -- becareful the ack is not tested, so it's useless for the moment
    Notif _ -> do
      let notifications = read s :: N.Notifications
      CM.forM_ notifications onNotification 
  return ()

parse :: String -> IO (Parsed)
parse s = do
  let maybeAck = TR.readMaybe s :: Maybe A.Ack
  let maybeNotifications = TR.readMaybe s :: Maybe N.Notifications

  let value = if (DM.isJust maybeAck)
        then Ack $ DM.fromJust maybeAck
        else Notif $ DM.fromJust maybeNotifications

  return value

connect :: IO (Maybe SIO.Handle)
connect = do
  handle <- IN.connectNetwork
  return $ handle

updateState :: St -> N.Notification -> St
updateState st (N.UpdateTasks ts) = set tasks ts st 
updateState st (N.UpdateTimeLogs tls) = set timeLogs tls st
