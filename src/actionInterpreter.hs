module ActionParser where

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


checkData :: GameData -> [String] -> Bool
checkData gameData ("locations":tail) = checkLocation (locations gameData) tail
checkData gameData ("backpack":tail) = checkObjects (backpack gameData) tail
checkData gameData ["flags", flag] = S.member flag (gameFlags gameData)
checkData _ _ = False

checkLocation :: Map LocationId Location -> [String] -> Bool
checkLocation locations [id] = member id locations
checkLocation locations (id:"objects":tail) = member id locations && checkObjects (objects(locations ! id)) tail 
checkLocation locations [id,"flags",flag] =  member id locations && S.member flag (locationFlags(locations ! id))
checkLocation _ _ = False

checkObjects :: Map ObjectId Object -> [String] -> Bool
checkObjects objects [id] = member id objects
checkObjects objects [id, "flags", flag] = member id objects && S.member flag (objectFlags(objects ! id))
checkObjects _ _= False



evalChange :: [String] -> ChangeType -> ChangeValue -> GameStateT ()
evalChange ("locations":tail) change value = modify (\s -> s {locations = (evalLocations tail change value (locations s))})
evalChange ["current"] Assign (StringValue value) = modify (\s -> s {current = value})
evalChange ("backpack":tail) change value = modify (\s -> s {backpack = (evalObjects tail change value (backpack s))})
evalChange ["flags", flag] change (StringValue value) = modify (\s -> s {gameFlags = (evalFlags change value (gameFlags s))})
evalChange _ _ _ = return ()

evalObjects ::[String] -> ChangeType -> ChangeValue -> Map ObjectId Object -> Map ObjectId Object
evalObjects [] Delete (StringValue value) = delete value
evalObjects [id] Assign (ObjectValue value) = insert id value
evalObjects (id:tail) change (StringValue value) =  (\objs -> adjustWithKey (\_ -> evalObject tail change value) id objs)
evalObjects _ _ _ = id

evalObject :: [String] -> ChangeType -> StringValue -> Object -> Object
evalObject ["info"] Assign value obj = obj{info = value}
evalObject ["interact"] Assign value obj = obj{interact = value}
evalObject ["use"] Assign value obj = obj{use = value}
evalObject ["flags",flag] change value obj = obj{objectFlags = evalFlags change value (objectFlags obj)}
evalObject _ _ _ obj = obj

evalLocations ::[String] -> ChangeType -> ChangeValue -> Map LocationId Location -> Map LocationId Location
evalLocations [] Delete (StringValue value) = delete value
evalLocations [id] Assign (ObjectValue value) = insert id (getMockLocation value)
evalLocations (id:tail) change value =  (\objs -> adjustWithKey (\_ -> evalLocation tail change value) id objs)
evalLocations _ _ _ = id

evalLocation :: [String] -> ChangeType -> ChangeValue -> Location -> Location
evalLocation ["description"] Assign (StringValue value) loc = loc{description = value}
evalLocation ["moves"] Delete (StringValue value) loc = loc{moves = delete value (moves loc)}
evalLocation ["moves",id] Assign (StringValue value) loc = loc{moves = insert id value (moves loc)}
evalLocation ("objects":tail) change value loc = loc{objects = (evalObjects tail change value (objects loc))}
evalLocation ["flags",flag] change (StringValue value) loc = loc{locationFlags = evalFlags change value (locationFlags loc)}
evalLocation _ _ _ l = l

evalFlags :: ChangeType -> Change -> S.Set Flag -> S.Set Flag
evalFlags Add value = S.insert value 
evalFlags Delete value = S.delete value 
evalFlags  _ _ = id



