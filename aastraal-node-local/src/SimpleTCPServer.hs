module SimpleTCPServer where
---
-- kreed131.blogspot.com/2011/07/tcp-haskell-networksocket.html
---

-- import Network.Socket hiding (send, recv)
-- import Network.Socket.ByteString
-- import Control.Concurrent (forkIO)
-- import qualified Data.ByteString.Char8 as B8
-- import System.Environment (getArgs)
-- import qualified GHC.Word as GW

-- main :: IO ()
-- main = do
--   [port'] <- getArgs
--   server $ Num (read port' :: GW.Word16)

-- server :: PortNumber -> IO ()
-- server port = withSocketsDo $ do
--                sock <- socket AF_INET Stream defaultProtocol
--                bindSocket sock (SockAddrInet port 0)
--                listen sock 5
--                sockHandler sock
--                sClose sock

-- sockHandler :: Socket -> IO ()
-- sockHandler sock = do
--   (sockh, _) <- accept sock
--   forkIO $ putStrLn "Client connected!" >> receiveMessage sockh
--   sockHandler sock

-- receiveMessage :: Socket -> IO ()
-- receiveMessage sockh = do
--   msg <- recv sockh 10
--   B8.putStrLn msg
--   if msg == B8.pack "q" || B8.null msg
--   then sClose sockh >> putStrLn "Client disconnected" 
--  else receiveMessage sockh
