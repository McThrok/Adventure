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
import Data.Maybe (isJust, fromJust)

import DataModel
import Parser
import Vocabulary

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
executeGameCommand (Just (Save, [path])) = saveGame path >> return True
executeGameCommand (Just (Inventory, [])) = get >>= showInventory >> return True
executeGameCommand (Just (Inventory, [id])) = get >>= showInventoryObject id>> return True
executeGameCommand _ = lift wrongCommand >> return True

saveGame :: String -> GameStateT ()
saveGame path = get >>= lift . encodeFile path  >> return ()

showInventory :: GameData -> GameStateT ()
showInventory gameData = printObjects $ map (\(k, v) -> k) $ toList $ backpack gameData
    where 
        printObjects (o:os) = lift (putStrLn o) >> printObjects os
        printObjects [] = return ()

showInventoryObject :: ObjectId -> GameData -> GameStateT ()
showInventoryObject id gameData = case  backpack gameData !? id of
    (Just obj) -> lift $ putStrLn $ info obj
    Nothing -> return ()

--  Go | Take | Use | Look | Interact | Inventory | Help deriving (Generic, Show)