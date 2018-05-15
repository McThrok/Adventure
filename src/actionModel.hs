module ActionModel
where

import Control.Monad.Trans.State
import Data.List
import Data.Map.Lazy hiding (foldl,map)
import DataModel

data Exp = Leaf String | Not Exp | And Exp Exp | Or Exp Exp

evalExp :: Exp -> Bool
evalExp (Leaf val) = True
evalExp (Not exp) = not $ evalExp exp
evalExp (And e1 e2) = evalExp e1 && evalExp e2
evalExp (Or e1 e2) = evalExp e1 || evalExp e2

type ActionType = [Instruction]
data Instruction = Print String | Change String ChangeType String | IfStatement Exp [Instruction]

data ChangeType = Add | Delete | Assign


checkDataContains :: GameData -> [String] -> Bool
checkDataContains gameData ("locations":tail) = checkLocationContains (locations gameData) tail
checkDataContains gameData ("backpack":tail) = checkObjectsContains (backpack gameData) tail
checkDataContains gameData ["flags", flag] = elem flag (gameFlags gameData)
checkDataContains _ _ = False

checkLocationContains :: Map LocationId Location -> [String] -> Bool
checkLocationContains locations [id] = member id locations
checkLocationContains locations (id:"objects":tail) = member id locations && checkObjectsContains (objects(locations ! id)) tail 
checkLocationContains locations [id,"flags",flag] =  member id locations && elem flag (locationFlags(locations ! id))
checkLocationContain _ _ = False

checkObjectsContains :: Map ObjectId Object -> [String] -> Bool
checkObjectsContains objects [id] = member id objects
checkObjectsContains objects [id, "flags", flag] = member id objects && elem flag (objectFlags(objects ! id))
checkObjectsContains _ _= False
