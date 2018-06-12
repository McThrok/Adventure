module ExpressionInterpreter (evalExp) where

import Control.Monad.Trans.State (get)
import qualified Data.Set as S (member)
import Data.Map.Lazy hiding (foldl, map)

import DataModel

-- | evaluate expression, value of single leaf if true if gmeStateT contains flag/object/location from leaf
evalExp :: Exp -> GameStateT Bool
evalExp (Leaf val) = get >>= return . (checkData val)
evalExp (Not exp) = evalExp exp >>= return . not
evalExp (And e1 e2) = (&&) <$> evalExp e1 <*> evalExp e2
evalExp (Or e1 e2) = (||) <$> evalExp e1 <*> evalExp e2

checkData :: [String] -> GameData -> Bool
checkData ("locations":tail) gameData = checkLocation (locations gameData) tail
checkData ("backpack":tail) gameData = checkObjects (backpack gameData) tail
checkData ["flags", flag] gameData = S.member flag (gameFlags gameData)
checkData _ _ = False

checkLocation :: Map LocationId Location -> [String] -> Bool
checkLocation locations [id] = member id locations
checkLocation locations (id:"objects":tail) = member id locations && checkObjects (objects(locations ! id)) tail 
checkLocation locations [id,"flags",flag] = member id locations && S.member flag (locationFlags(locations ! id))
checkLocation _ _ = False

checkObjects :: Map ObjectId Object -> [String] -> Bool
checkObjects objects [id] = member id objects
checkObjects objects [id, "flags", flag] = member id objects && S.member flag (objectFlags(objects ! id))
checkObjects _ _= False

