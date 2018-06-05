module Parser where

import Text.ParserCombinators.Parsec
import Data.Map.Lazy hiding (foldl,map)
import qualified Data.Set as S
import Data.Either.Combinators

import DataModel
import ParserHelper

parseAdventureFile :: String -> Maybe GameData
parseAdventureFile input = rightToMaybe $ parse adventureFile "(unknown)" input

adventureFile :: GenParser Char st GameData
adventureFile = do
    getWhites
    locations <- getString "locations:" >> getMap getLocationBody
    comma
    current <- getString "current:" >> getWord
    comma
    backpack <- getString "backpack:" >> getObjectsMap
    comma
    actions <- getString "interactions:" >> getMap getActionBody
    comma
    useActions <- getString "use actions:" >> getMap getUseActionBody
    comma
    flags <- getString "flags:" >> getFlagSet
    eof
    return (GameData locations current backpack actions useActions flags)

getLocationBody :: GenParser Char st Location
getLocationBody = do
    getString "("
    info <- getInfo
    comma
    moves <- getMap getWord
    comma
    objects <- getObjectsMap
    comma
    flags <- getFlagSet
    getString ")"
    return (Location info moves objects flags)

getUseActionBody :: GenParser Char st (ObjectId, Action)
getUseActionBody = do
    getString "("
    id <- getWord
    comma
    action <- getActionBody
    getString ")"
    return (id, action)

getObjectsMap :: GenParser Char st (Map ObjectId Object) 
getObjectsMap = getMap getObjectBody

getObjectBody :: GenParser Char st Object
getObjectBody = do
    getString "("
    info <- getInfo
    comma
    interAction <- getActionId
    comma
    useAction <- getActionId
    comma
    flags <- getFlagSet
    getString ")"
    return (Object info interAction useAction flags)

getActionId ::  GenParser Char st String
getActionId = getWord <|> (getWhites >> return "")

getFlagSet :: GenParser Char st (S.Set Flag)
getFlagSet = do 
    getString "["
    result <- sepBy getWord (comma)
    getString "]"
    return (S.fromList result)



    
getActionBody :: GenParser Char st Action
getActionBody = do 
    getString "{"
    instructions <- many getInstruction
    getString "}"
    return instructions

getInstruction :: GenParser Char st Instruction
getInstruction = getPrint  <|>  getIf <|> getChange

getPrint :: GenParser Char st Instruction
getPrint = getString "print" >> getInfo >>= return . Print

getChange :: GenParser Char st Instruction
getChange = do
    prop <- getProperty
    change <- getChangeType
    value <- getValue
    return (Change prop change value)

getChangeType:: GenParser Char st ChangeType
getChangeType = (getString "=" <|> getString "-=" <|> getString "+=" ) >>= (\w-> case w of
        "=" -> return Assign
        "+=" -> return Add
        "-=" -> return Delete)
            
getValue :: GenParser Char st ChangeValue
getValue = (getString "id" >> getWord >>= return . StringValue)
    <|> (getString "info" >> getInfo >>= return . StringValue) 
    <|> (getString "location" >> getLocationBody >>= return . LocationValue)
    <|> (getString "object" >> getObjectBody >>= return . ObjectValue)




getIf :: GenParser Char st Instruction
getIf = do 
    getString "if"
    exp <- getExp
    action <- getActionBody
    return (IfStatement exp action)

getExp :: GenParser Char st Exp
getExp = getBinaryExp <|> getNotExp <|> getLeaf

getBinaryExp :: GenParser Char st Exp
getBinaryExp = do
    getString "("
    a <- getExp
    op <- getString "||" <|> getString "&&"
    b <- getExp
    getString ")"
    case op of
        "||" -> return (Or a b)
        "&&" -> return (And a b)

getNotExp :: GenParser Char st Exp
getNotExp = getString "!" >> getExp >>= return . Not

getLeaf :: GenParser Char st Exp
getLeaf = getProperty >>= return . Leaf

