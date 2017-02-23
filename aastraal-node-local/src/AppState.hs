module AppState where

import Command
import AppEvent

data St = St { commands :: [Command]
             , events :: [Event]
             }
          
