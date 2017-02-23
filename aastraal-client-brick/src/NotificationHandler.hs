module NotificationHandler where

import qualified IoNetwork as IN
import qualified System.IO as SIO
import qualified Notification as N
import qualified Ack as A 
import qualified Text.Read as TR
import qualified Data.Maybe as DM
import Control.Lens
import AppState

treatEachMsg :: SIO.Handle -> (N.Notification -> IO ()) -> IO ()
treatEachMsg handle onNotification = do
    IN.receiveOverNetwork (treatMessage onNotification) handle 

data Parsed = Ack A.Ack
              | Notif N.Notification

treatMessage :: (N.Notification -> IO ()) -> String -> IO ()
treatMessage onNotification s = do
  parsed <- parse s 
  case parsed of
    Ack _ -> return ()    -- becareful the ack is not tested, so it's useless for the moment
    Notif _ -> do
      let notification = read s :: N.Notification
      onNotification notification
  return ()

parse :: String -> IO (Parsed)
parse s = do
  let maybeAck = TR.readMaybe s :: Maybe A.Ack
  let maybeNotification = TR.readMaybe s :: Maybe N.Notification

  let value = if (DM.isJust maybeAck)
        then Ack $ DM.fromJust maybeAck
        else Notif $ DM.fromJust maybeNotification

  return value

connect :: IO (Maybe SIO.Handle)
connect = do
  handle <- IN.connectNetwork
  return $ handle

updateState :: St -> N.Notification -> St
updateState st (N.UpdateTasks ts) = set tasks ts st 

