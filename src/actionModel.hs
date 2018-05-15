module ActionModel
where

import Control.Monad.Trans.State
import qualified Data.List as L
import qualified Data.Set as S
import Data.Map.Lazy hiding (foldl,map)
import Prelude hiding (interact)

import DataModel


evalExp :: Exp -> Bool
evalExp (Leaf val) = True
evalExp (Not exp) = not $ evalExp exp
evalExp (And e1 e2) = evalExp e1 && evalExp e2
evalExp (Or e1 e2) = evalExp e1 || evalExp e2


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
evalChange ("locations":tail) change value = modify (\s -> s {locations = (evalChangeLocations tail change value (locations s))})
evalChange ["current"] Assign value = modify (\s -> s {current = value})
evalChange ("backpack":tail) change value = modify (\s -> s {backpack = (evalChangeObjects tail change value (backpack s))})
evalChange ["flags", flag] change value = modify (\s -> s {gameFlags = (evalChangeFlags change value (gameFlags s))})
evalChange _ _ _ = return ()

evalChangeObjects ::[String] -> ChangeType -> String -> Map ObjectId Object -> Map ObjectId Object
evalChangeObjects [] Delete value = delete value
evalChangeObjects [id] Assign value = insert id (getMockObject value)
evalChangeObjects (id:tail) change value =  (\objs -> adjustWithKey (\_ -> evalChangeObject tail change value) id objs)
evalChangeObjects _ _ _ = id

evalChangeObject :: [String] -> ChangeType -> String -> Object -> Object
evalChangeObject ["info"] Assign value obj = obj{info = value}
evalChangeObject ["interact"] Assign value obj = obj{interact = value}
evalChangeObject ["use"] Assign value obj = obj{use = value}
evalChangeObject ["flags",flag] change value obj = obj{objectFlags = evalChangeFlags change value (objectFlags obj)}
evalChangeObject _ _ _ obj = obj

evalChangeLocations ::[String] -> ChangeType -> String -> Map LocationId Location -> Map LocationId Location
evalChangeLocations [] Delete value = delete value
evalChangeLocations [id] Assign value = insert id (getMockLocation value)
evalChangeLocations (id:tail) change value =  (\objs -> adjustWithKey (\_ -> evalChangeLocation tail change value) id objs)
evalChangeLocations _ _ _ = id

evalChangeLocation :: [String] -> ChangeType -> String -> Location -> Location
evalChangeLocation ["description"] Assign value loc = loc{description = value}
evalChangeLocation ["moves"] Delete value loc = loc{moves = delete value (moves loc)}
evalChangeLocation ["moves",id] Assign value loc = loc{moves = insert id value (moves loc)}
evalChangeLocation ("objects":tail) change value loc = loc{objects = (evalChangeObjects tail change value (objects loc))}
evalChangeLocation ["flags",flag] change value loc = loc{locationFlags = evalChangeFlags change value (locationFlags loc)}
evalChangeLocation _ _ _ l = l

getMockObject :: String -> Object
getMockObject s = Object "" "" "" S.empty

getMockLocation :: String -> Location
getMockLocation s = Location "" (fromList []) (fromList []) S.empty

evalChangeFlags :: ChangeType -> String -> S.Set Flag -> S.Set Flag
evalChangeFlags Add value = S.insert value 
evalChangeFlags Delete value = S.delete value 
evalChangeFlags  _ _ = id



