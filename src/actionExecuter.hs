module ActionExecuter where

import Control.Monad.State (lift)

import DataModel
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


