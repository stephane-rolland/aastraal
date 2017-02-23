{-# LANGUAGE TemplateHaskell #-}

module Task where

import Control.Lens

type TaskName = String
type TaskUuid = String
type TaskDescription = String
type TaskWhy = String
type TaskParent = String
type Tasks = [Task]

data TaskStatus =   ToDo
              | Urgent
              | Doing
              | Done
              | Delayed
              | Cancelled
              | Problem
              deriving (Show,Read)

type TaskAssurance = Int  -- 0 -> 100    i.e. 100% when one is completely sure to succeede

data TaskCynefin =   Unknown -- No idea about the complexity involved : means a lack of information
               | Chaos       -- no one ever did it probably
               | Complex     -- someone else may have done it
               | Complicated -- someone else I knox has done it, several parts 
               | Obvious     -- I already did it, I really know how to do it
               deriving (Show,Read)

type TaskValue = Int -- 0 -> 100

type TaskEstimate = Int -- hours

type TaskPerturbation = Int -- 0 -> 100i  i.e. 30% means 1/3 lost in perturbations twitter fcbk etc..
 
data Task = Task
  {
    _name :: TaskName,
    _uuid :: TaskUuid,
    _description :: TaskDescription,
    _parent :: TaskParent,
    _why :: TaskWhy,
    _status :: TaskStatus,
    _assurance :: TaskAssurance,
    _cynefin :: TaskCynefin,
    _value :: TaskValue,
    _estimate :: TaskEstimate,
    _perturbation :: TaskPerturbation
  } deriving (Show,Read)


makeLenses ''Task

mkDefaultTask :: Task
mkDefaultTask = Task "" "" "" "" "" ToDo 100 Obvious 0 1 30
