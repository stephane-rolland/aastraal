module BasicConcurrentServer where

import Network
import System.IO

import Command
import DecisionEngine
import Ack
import qualified Control.Monad as CM

main :: DecisionEngine -> IO ()
main decisionEngine = withSocketsDo $ do
    socketCommands <- listenOn $ portNumberCommands
    listenCommands socketCommands decisionEngine 

listenCommands :: Socket -> DecisionEngine -> IO ()
listenCommands socketCommands decisionEngine = do
   (handleCommands, _, _) <- accept socketCommands
   respondToCommands decisionEngine handleCommands

notifier :: Handle -> SubscriberNotification
notifier h s = do
     hPutStrLn h s
     hFlush h

respondToCommands :: DecisionEngine -> Handle -> IO()     
respondToCommands decisionEngine h = CM.forever $ do 
       input <- hGetLine h
       let inputCommand = read input :: Command
       putStrLn $ "Msg has been received = " ++ (show inputCommand)
       let ack = show Ack
       hPutStrLn h ack
       hFlush h

       putStrLn $ "Ack sent back"

       decisionEngine inputCommand $ notifier h      

       -- hClose h 
       putStrLn "Decided"

portNumberCommands :: PortID
portNumberCommands = PortNumber 33334
-- portNumberSubscribers = 33335






