-- {-# LANGUAGE OverloadedStrings #-}
module BasicConcurrentServerB where


 
-- import Data.ByteString.Char8
 
-- import Network hiding (accept)
-- import qualified Network.Socket as NetworkSocket
-- import Network.Socket.ByteString (sendAll)
-- import Control.Concurrent
 
-- main = withSocketsDo $ do
--    sock <- listenOn $ PortNumber 5002
--    loop sock
 
-- loop sock = do
--    (conn, _) <- NetworkSocket.accept sock
--    _ <- forkIO $ body conn
--    loop sock
--  where
--   body c = do sendAll c msg
--               NetworkSocket.sClose c
 
-- msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"



