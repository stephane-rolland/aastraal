-- Echo client program
-- taken from https://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket-ByteString.html

module IoNetwork where

import Network.Socket hiding (recv)
-- import Network.Socket.ByteString (recv, sendAll)
-- import qualified Data.ByteString.Char8 as C
import qualified Network as N
import qualified System.IO as SIO

import Command
import qualified Control.Monad as ControlMonad

-- just for imports while I have the wifi
-- import Network
-- import Control.Concurrent
-- import Control.Applicative
-- import Data.Function
-- import Data.List
-- import Data.Map
-- import System.IO
-- import Data.ByteString.Char8
-- import System.Event
-- import System.Posix
-- import System.Posix.IO
-- import System
-- import Database.SQLite.Simple

-- ioNetwork :: IO ()
-- ioNetwork = withSocketsDo $
--    do addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "33334")
--       let serveraddr = head addrinfos
--       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
--       connect sock (addrAddress serveraddr)
--       sendAll sock $ C.pack "Hello, world!"
--       msg <- recv sock 1024
--       sClose sock
--       putStr "Received "
--       C.putStrLn msg



connectionPort :: N.PortID
connectionPort = N.PortNumber 33334

connectNetwork :: IO(Maybe SIO.Handle)
connectNetwork = withSocketsDo $
  do
    handle <- N.connectTo "localhost" connectionPort
    SIO.hSetBuffering handle SIO.LineBuffering
    return $ Just handle

sendOverNetwork :: SIO.Handle -> Command -> IO ()
sendOverNetwork handle cmd = do
    SIO.hPutStrLn handle $ show cmd

receiveOverNetwork :: (String -> IO()) -> SIO.Handle -> IO ()
receiveOverNetwork treatMessage handle = do
  _ <- ControlMonad.forever $ do
    msg <- getNextMessage handle
    treatMessage msg
  return ()

getNextMessage :: SIO.Handle -> IO(String)
getNextMessage handle = do
    msg <- SIO.hGetLine handle
    return msg

  
