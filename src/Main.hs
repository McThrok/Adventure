module Main where

import Data.List.Split
import Data.List
import Data.Map.Lazy hiding (foldl,map)
import Control.Monad
import Prelude hiding (Word)

main::IO()
main = mainLoop

mainLoop::IO()
mainLoop = do
    command <- getLine
    result <- executeCommand command
    if result then mainLoop else return ()

data Word = Go|Take|Quit deriving(Eq,Show)

data Location = Kitchen|BedRoom

wordDict:: Map String Word
wordDict = fromList [("go",Go),("take",Take),("quit",Quit)]

executeCommand::String->IO Bool
executeCommand command = process $ getWords (splitOn " " command)

getWords::[String]->Maybe [Word]
getWords [] = Just []
getWords (s:ss) = (:) <$> (wordDict!? s) <*> (getWords ss)

process::Maybe [Word]-> IO Bool
process Nothing = return True;
process (Just []) = return True;
process (Just [Quit]) = return False;
process (Just words) = processWords words >> return True

processWords::[Word]->IO ()
processWords words = putStrLn (show words) >> return ()
