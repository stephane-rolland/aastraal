{-# LANGUAGE TemplateHaskell #-}

module TimeLog where

import Control.Lens
import Task
import qualified Data.Time.Clock as DTC

type TimeLogs = [TimeLog]

data TimeLog = TimeLog
  {
    _relatedTaskUuid :: TaskUuid,
    _time :: DTC.UTCTime,
    _comment :: String
  }
  deriving (Show,Read,Ord,Eq)

makeLenses ''TimeLog



