module ActionExecuter where

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
import Parser
import Vocabulary
import ChangeActionInterpreter (evalChange)
import ExpressionInterpreter (evalExp)

executeAction :: Action -> GameStateT ()
executeAction [] = return ()
executeAction (a:as) = executeInstruction a >> executeAction as

executeInstruction :: Instruction -> GameStateT ()
executeInstruction (Print str) = lift (putStrLn str)
executeInstruction (Change property changeType value) = evalChange property changeType value
executeInstruction (IfStatement exp action) = do
    expValue <- evalExp exp 
    if expValue then executeAction action else return ()


-- data Instruction = Print String | Change [String] ChangeType ChangeValue | IfStatement Exp Action deriving (Generic, Show)
-- data ChangeType = Add | Delete | Assign deriving (Generic, Show)
-- data ChangeValue =  StringValue String | ObjectValue Object | LocationValue Location deriving (Generic, Show)
-- data Exp = Leaf [String] | Not Exp | And Exp Exp | Or Exp Exp deriving (Generic, Show)
