module Main where

import qualified Data.Text as T
import Data.List
import Data.Map.Lazy hiding (foldl,map)
import Control.Monad
import System.IO
import System.IO.Error
import Prelude hiding (Word)
import Data.Binary
import Data.Binary.Get
import Control.Monad.Trans.State.Lazy

import DataModel
import Parser
import Vocabulary

-- main::IO ()
-- -- main =  putStrLn "Hello World" >> return ()
-- main = withFile "qwe.txt" ReadMode process

-- process :: Handle -> IO ()
-- process h = do
--     line <- hGetContents  h
--     parsed <- return (parseAdventureFile line)
--     putStrLn (show parsed)
--     withFile "qqqqqqqqqq.txt" WriteMode (saveFile line)
--     return ()

-- saveFile :: String -> Handle -> IO ()
-- saveFile content h = do
--     hPutStr h content

    

main :: IO ()
main = mainMenu

mainMenu ::IO ()
mainMenu =  getCommand >>= executeMenuCommand


getCommand :: IO (Maybe (Command, [String]))
getCommand =  getLine >>= getWords  >>= return . parseCommand 
    where getWords = return . (map T.unpack) . (T.splitOn (T.pack " ")) . T.pack

parseCommand :: [String] -> Maybe (Command, [String])
parseCommand [] = Nothing
parseCommand (s:ss) =  vocCommand !? s >>= (\cmd -> Just (cmd, ss))

executeMenuCommand :: Maybe (Command, [String]) -> IO ()
executeMenuCommand (Just (Quit, [])) = return ()
executeMenuCommand (Just (New, [path])) = startGame path
executeMenuCommand (Just (Load, [path])) = loadGame path
executeMenuCommand _ =  wrongCommand >> mainMenu

wrongCommand :: IO ()
wrongCommand = putStrLn "Huh?"

startGame :: String -> IO ()
startGame path = do
    parsedContent <- withFile path ReadMode getGame
    case parsedContent of
        Nothing -> putStrLn "an error occurs during loading" >> mainMenu
        (Just gameData) -> putStrLn "game started" >> runGame gameData
    where getGame h = hGetContents h >>= return . parseAdventureFile


loadGame :: String -> IO ()
loadGame path = do
    loadedData <- decodeFileOrFail path :: IO (Either (ByteOffset, String) GameData)
    case loadedData of
        (Left _) -> putStrLn "an error occurs during loading" >> mainMenu
        (Right gameData) -> putStrLn "game loaded" >> runGame gameData

runGame :: GameData -> IO ()
runGame gameData = evalStateT gameLoop gameData

gameLoop :: GameStateT ()
gameLoop = return ()


-- loadGame path = withFile path ReadMode loadStateFromFile >> return ()

-- loadStateFromFile :: Handle -> IO (Maybe GameData)
-- loadStateFromFile h = do
--     content <- hGetContents  h 
--     return (decode content :: GameData)
--     line <- hGetContents  h
--     parsed <- return (parseAdventureFile line)
--     putStrLn (show parsed)  
--     withFile "qqqqqqqqqq.txt" WriteMode (saveFile line)
--     return ()



-- mainLoop::IO()
-- mainLoop = do
--     command <- getLine
--     result <- executeCommand command
--     if result then mainLoop else return ()

-- data Word = Go|Take|Quit deriving(Eq,Show)

-- data Location = Kitchen|BedRoom

-- wordDict:: Map String Word
-- wordDict = fromList [("go",Go),("take",Take),("quit",Quit)]

-- executeCommand::String->IO Bool
-- executeCommand command = process $ getWords (splitOn " " command)

-- getWords::[String]->Maybe [Word]
-- getWords [] = Just []
-- getWords (s:ss) = (:) <$> (wordDict!? s) <*> (getWords ss)

-- process::Maybe [Word]-> IO Bool
-- process Nothing = return True;
-- process (Just []) = return True;
-- process (Just [Quit]) = return False;
-- process (Just words) = processWords words >> return True

-- processWords::[Word]->IO ()
-- processWords words = putStrLn (show words) >> return ()
