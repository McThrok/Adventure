module GameCommandExecuter where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Map.Lazy hiding (foldl,map)
import System.IO
import System.IO.Error
import Prelude hiding (Word)
import Data.Binary hiding (get)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.State.Lazy as MTSL
import Control.Monad.State (lift)
import Data.Maybe (isJust, fromJust)

import DataModel
import Vocabulary
import ActionExecuter


wrongCommand :: IO ()
wrongCommand = putStrLn "Huh?"

checkAndExecute :: Maybe (Command, [String]) -> GameStateT Bool
checkAndExecute command = get >>=(lift . putStrLn . show) >> (&&) <$>  (not <$> checkGameFinished) <*> executeGameCommand command

checkGameFinished :: GameStateT Bool
checkGameFinished = get >>= return . elem "gameFinished" . gameFlags

executeGameCommand :: Maybe (Command, [String]) -> GameStateT Bool
executeGameCommand (Just (Quit, [])) = return False
executeGameCommand (Just (Save, [path])) = saveGame path >> return True
executeGameCommand (Just (Inventory, [])) = get >>= showInventory >> return True
executeGameCommand (Just (Inventory, [id])) = get >>= showInventoryObject id>> return True
executeGameCommand (Just (Go, [id])) = get >>= move id >> return True
executeGameCommand (Just (Look, [])) = get >>= lookAround >> return True
executeGameCommand (Just (Look, [id])) = get >>= look id >> return True
executeGameCommand (Just (Take, [id])) = get >>= takeObject id >> return True
executeGameCommand (Just (Interact, [id])) = get >>= interactWith id >> return True
executeGameCommand (Just (Use, [id,"on",idOn ])) = get >>= useObject id idOn>> return True
executeGameCommand (Just (Help, [])) = lift  printHelp >> return True
executeGameCommand _ = lift wrongCommand >> return True

saveGame :: String -> GameStateT ()
saveGame path = get >>= lift . encodeFile path  >> return ()

showInventory :: GameData -> GameStateT ()
showInventory gameData = printObjects $ map (\(k, v) -> k) $ toList $ backpack gameData
    where 
        printObjects (o:os) = lift (putStrLn o) >> printObjects os
        printObjects [] = return ()

showInventoryObject :: ObjectId -> GameData -> GameStateT ()
showInventoryObject id gameData = case backpack gameData !? id of
    Nothing -> lift wrongCommand
    (Just obj) -> lift $ putStrLn $ objectInfo obj

move :: Direction -> GameData -> GameStateT ()
move dir gameData = case moves (locations gameData ! (current gameData)) !? dir of
    Nothing -> lift wrongCommand
    (Just loc) -> do
        modify (\s -> s {current = loc}) 
        s <- get
        lift $ putStrLn $ locationInfo  $ locations s ! loc

look :: ObjectId -> GameData -> GameStateT ()
look id gameData = case getObjectsInCurrLoc gameData !? id of
    Nothing -> lift wrongCommand
    (Just obj) -> lift $ putStrLn $ objectInfo obj

lookAround :: GameData -> GameStateT ()
lookAround  gameData = lift $ putStrLn $ locationInfo $ locations gameData ! (current gameData)

takeObject :: ObjectId -> GameData -> GameStateT ()
takeObject id gameData = case (getObjectsInCurrLoc gameData) !? id of
    Nothing -> lift wrongCommand
    (Just obj) -> do
        if elem "canBeTaken" (objectFlags obj)
            then modify (\s -> s {backpack = (delete id . insert id obj) (backpack s)}) >> lift (putStrLn ("You took " ++ id))
            else lift wrongCommand

interactWith :: ObjectId -> GameData -> GameStateT ()
interactWith id gameData = case getInterAction id gameData of
            Nothing -> lift wrongCommand
            (Just action) -> executeAction action

getInterAction :: ObjectId -> GameData -> Maybe (Action)
getInterAction id gameData = getObjectsInCurrLoc gameData !? id >>= return . interAction >>= (interActions gameData !?)

useObject :: ObjectId -> ObjectId -> GameData -> GameStateT ()
useObject id idOn gameData =  case getUseAction idOn gameData of
                    Nothing -> lift wrongCommand
                    (Just (key, action)) -> if id == key
                        then executeAction action
                        else lift wrongCommand

getUseAction :: ObjectId -> GameData -> Maybe (ObjectId, Action)
getUseAction idOn gameData = getObjectsInCurrLoc gameData !? idOn >>= return . useAction >>= (useActions gameData !?)

getObjectsInCurrLoc :: GameData -> Map ObjectId Object
getObjectsInCurrLoc gameData = objects (locations gameData ! (current gameData))

printHelp :: IO ()
printHelp = putStrLn "this help is not helpful"