module Game (mainMenu) where

import qualified Data.Text as T

import Data.Map.Lazy hiding (foldl,map)
import System.IO
import System.IO.Error
import Data.Binary hiding (get)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.State.Lazy
import Control.Monad.State (lift)
import Data.Maybe (isJust, fromJust)

import DataModel
import Parser
import Vocabulary
import GameCommandExecuter

-- | run game
mainMenu ::IO ()
mainMenu = getCommand >>= executeMenuCommand >>= (\result -> if result then mainMenu else return())

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
executeMenuCommand (Just (Help, [])) = showMenuHelp
executeMenuCommand _ =  wrongCommand >> return True

startNewGame :: String -> IO Bool
startNewGame path = do
    parsedContent <- withFile path ReadMode getGame
    case parsedContent of
        Nothing -> putStrLn "an error occurs during loading"
        (Just gameData) -> putStrLn "game started" >> runGame gameData
    return True
        where getGame h = hGetContents h  >>=  (return $!) .  parseAdventureFile 

loadGame :: String -> IO Bool
loadGame path = do
    loadedData <- decodeFileOrFail path :: IO (Either (ByteOffset, String) GameData)
    case loadedData of
        (Left _) -> putStrLn "an error occurs during loading"
        (Right gameData) -> putStrLn "game loaded" >> runGame gameData 
    return True

showMenuHelp :: IO Bool
showMenuHelp = putStrLn "new [path] - start new game, [path]  path to the game\n\
    \load [path] - load game from [path]\n\
    \help - show all commands\n\
    \quit - exit" 
    >> return True

runGame :: GameData -> IO ()
runGame gameData = evalStateT gameLoop gameData

gameLoop :: GameStateT ()
gameLoop = lift getCommand >>= checkAndExecute >>= (\result -> if result then gameLoop else return())
