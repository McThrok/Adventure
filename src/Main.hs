module Main where

import qualified Data.Text as T
import Data.List
import Data.Map.Lazy hiding (foldl,map)
import System.IO
import System.IO.Error
import Prelude hiding (Word)
import Data.Binary hiding (get)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.State.Lazy
import Control.Monad.State (lift)

import DataModel
import Parser
import Vocabulary

-- saveFile :: String -> Handle -> IO ()
-- saveFile content h = do
--     hPutStr h content

main :: IO ()
main = mainMenu

mainMenu ::IO ()
mainMenu =  getCommand >>= executeMenuCommand >>= (\result -> if result then mainMenu else return())


getCommand :: IO (Maybe (Command, [String]))
getCommand =  getLine >>= getWords  >>= return . parseCommand 
    where getWords = return . (map T.unpack) . (T.splitOn (T.pack " ")) . T.pack

parseCommand :: [String] -> Maybe (Command, [String])
parseCommand [] = Nothing
parseCommand (s:ss) =  vocCommand !? s >>= (\cmd -> Just (cmd, ss))

executeMenuCommand :: Maybe (Command, [String]) -> IO Bool
executeMenuCommand (Just (Quit, [])) = return False
executeMenuCommand (Just (New, [path])) = startNewGame path
executeMenuCommand (Just (Load, [path])) = loadGame path
executeMenuCommand _ =  wrongCommand >> return True

wrongCommand :: IO ()
wrongCommand = putStrLn "Huh?"

startNewGame :: String -> IO Bool
startNewGame path = do
    parsedContent <- withFile path ReadMode getGame
    case parsedContent of
        Nothing -> putStrLn "an error occurs during loading"
        (Just gameData) -> putStrLn "game started" >> runGame gameData
    return True
        where getGame h = hGetContents h >>= return . parseAdventureFile

loadGame :: String -> IO Bool
loadGame path = do
    loadedData <- decodeFileOrFail path :: IO (Either (ByteOffset, String) GameData)
    case loadedData of
        (Left _) -> putStrLn "an error occurs during loading"
        (Right gameData) -> putStrLn "game loaded" >> runGame gameData 
    return True

runGame :: GameData -> IO ()
runGame gameData = evalStateT gameLoop gameData

gameLoop :: GameStateT ()
gameLoop = lift getCommand >>= executeGameCommand >>= (\result -> if result then gameLoop else return())

executeGameCommand :: Maybe (Command, [String]) -> GameStateT Bool
executeGameCommand (Just (Quit, [])) = return False
executeGameCommand (Just (Save, [path])) = saveGame path
executeGameCommand _ = lift wrongCommand >> return True

saveGame :: String -> GameStateT Bool
saveGame path = get >>= lift . encodeFile path  >> return True

-- saveData :: GameData -> Handle -> IO ()
-- saveData gameData h =  hPutStr  h $ unpack $ encode gameData



-- data Command = Go | Take | Use | Look | Interact | Inventory | Save | Load | New | Quit | Help deriving (Generic, Show)

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
