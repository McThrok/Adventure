module IfStatementInterpreter where

import Control.Monad.Trans.State
import qualified Data.List as L
import qualified Data.Set as S
import Data.Map.Lazy hiding (foldl,map)
import Prelude hiding (interact)

import DataModel


-- evalExp :: Exp -> Bool
-- evalExp (Leaf val) = True
-- evalExp (Not exp) = not $ evalExp exp
-- evalExp (And e1 e2) = evalExp e1 && evalExp e2
-- evalExp (Or e1 e2) = evalExp e1 || evalExp e2

evalIfStatement :: Exp -> GameData -> Bool
evalIfStatement _ _ = True

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


-- data Instruction = Print String | Change [String] ChangeType ChangeValue | IfStatement Exp Action deriving (Generic, Show)
-- data ChangeType = Add | Delete | Assign deriving (Generic, Show)
-- data ChangeValue =  StringValue String | ObjectValue Object | LocationValue Location deriving (Generic, Show)
-- data Exp = Leaf [String] | Not Exp | And Exp Exp | Or Exp Exp deriving (Generic, Show)
