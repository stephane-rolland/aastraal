{-# LANGUAGE BangPatterns #-}

module FileDataBase
(
    dbLoad
  , dbSave
)
where

import qualified Control.DeepSeq as CDS

-- mkTask :: String -> T.Task
-- mkTask str = T.Task name uuid description parent why
--   where
--     fields = foldr reducer [] str
--     name : uuid : description : parent : why : [] = fields

-- reducer :: Char -> [String] -> [String]
-- reducer c []         = [[c]]
-- reducer '§' (x:xs)   = case x of
--                                [] -> "" : xs
--                                ss -> "" : ss : xs
-- reducer c (x:xs)     = case x of
--                               []     -> [c] : xs
--                               ss     -> ( c : ss ) : xs 

-- dbLoad :: IO (T.Tasks)
-- dbLoad = do
--   putStrLn $ "FileDb=" ++ dbFileName
--   content <- readFile dbFileName
--   putStrLn $ "Db contains: \n" ++ content
--   let allLines = lines content
--   let tasks = map 
--   putStrLn $ show $ head allLines
--   let task = mkTask $ head allLines
--   return $ [task]

-- dbSave :: T.Tasks -> IO ()
-- dbSave ts = undefined

dbLoad :: (String -> a) -> String -> IO ([a])
dbLoad reader filename = do
  content <- readFile filename
  let !strictContent = content `CDS.deepseq` content
  let allLines =  lines $ strictContent
  let allItems = map reader allLines
  return allItems

dbSave :: (a -> String) -> [a] -> String -> IO ()
dbSave writer as filename = do
  let allSerialized = map writer as
  let content = unlines allSerialized
  writeFile filename content
  return ()



