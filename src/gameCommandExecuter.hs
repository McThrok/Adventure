module GameCommandExecuter where

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


wrongCommand :: IO ()
wrongCommand = putStrLn "Huh?"

executeGameCommand :: Maybe (Command, [String]) -> GameStateT Bool
executeGameCommand (Just (Quit, [])) = return False
executeGameCommand (Just (Save, [path])) = saveGame path >> return True
executeGameCommand (Just (Inventory, [])) = get >>= showInventory >> return True
executeGameCommand (Just (Inventory, [id])) = get >>= showInventoryObject id>> return True
executeGameCommand (Just (Go, [id])) = get >>= move id>> return True
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
    Nothing -> return ()
    (Just obj) -> lift $ putStrLn $ info obj

move :: Direction -> GameData -> GameStateT ()
move dir gameData = case moves (locations gameData ! (current gameData)) !? dir of
    Nothing -> return ()
    (Just loc) -> do
        modify (\s -> s{current = loc}) 
        s <- get
        lift $ putStrLn $ description  $ locations s ! loc

        


-- | Take | Use | Look | Interact | Help 