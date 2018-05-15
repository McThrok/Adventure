module ActionModel
where

import Control.Monad.Trans.State
import qualified Data.List as L
import qualified Data.Set as S
import Data.Map.Lazy hiding (foldl,map)
import DataModel

data Exp = Leaf [String] | Not Exp | And Exp Exp | Or Exp Exp

evalExp :: Exp -> Bool
evalExp (Leaf val) = True
evalExp (Not exp) = not $ evalExp exp
evalExp (And e1 e2) = evalExp e1 && evalExp e2
evalExp (Or e1 e2) = evalExp e1 || evalExp e2

data Instruction = Print String | Change [String] ChangeType String | IfStatement Exp [Instruction]

data ChangeType = Add | Delete | Assign

checkDataContains :: GameData -> [String] -> Bool
checkDataContains gameData ("locations":tail) = checkLocationContains (locations gameData) tail
checkDataContains gameData ("backpack":tail) = checkObjectsContains (backpack gameData) tail
checkDataContains gameData ["flags", flag] = S.member flag (gameFlags gameData)
checkDataContains _ _ = False

checkLocationContains :: Map LocationId Location -> [String] -> Bool
checkLocationContains locations [id] = member id locations
checkLocationContains locations (id:"objects":tail) = member id locations && checkObjectsContains (objects(locations ! id)) tail 
checkLocationContains locations [id,"flags",flag] =  member id locations && S.member flag (locationFlags(locations ! id))
checkLocationContain _ _ = False

checkObjectsContains :: Map ObjectId Object -> [String] -> Bool
checkObjectsContains objects [id] = member id objects
checkObjectsContains objects [id, "flags", flag] = member id objects && S.member flag (objectFlags(objects ! id))
checkObjectsContains _ _= False



evalChange :: [String] -> ChangeType -> String -> GameStateT ()
evalChange  ["flags", flag] change value = modify (\s -> s {gameFlags = (evalChangeFlags (gameFlags s) change value)})


evalChangeFlags :: S.Set Flag -> ChangeType -> String -> S.Set Flag 
evalChangeFlags flags Add value = S.insert value flags
evalChangeFlags flags Delete value = S.delete value flags
evalChangeFlags flags _ _ = flags



