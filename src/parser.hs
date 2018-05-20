module Parser where

import Text.ParserCombinators.Parsec
import Data.Map.Lazy hiding (foldl,map)
import qualified Data.Set as S
import Data.Either.Combinators

import DataModel

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
    description <- getInfo
    comma
    moves <- getMap getWord
    comma
    objects <- getObjectsMap
    comma
    flags <- getFlagSet
    getString ")"
    return (Location description moves objects flags)

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
    interAction <- getWord
    comma
    useAction <- getWord
    comma
    flags <- getFlagSet
    getString ")"
    return (Object info interAction useAction flags)

getWord :: GenParser Char st String
getWord = do
    word <-  many1 (alphaNum <|> (char '_'))
    getWhites
    return word

getString :: String-> GenParser Char st String
getString str = do
    string str
    getWhites
    return str

getWhites :: GenParser Char st String
getWhites = many (oneOf "\n\t ")

comma :: GenParser Char st String
comma = getString ","

getInfo ::  GenParser Char st String
getInfo = do
    string "\""
    content <- many (noneOf "\"") 
    string "\""
    return content

getFlagSet :: GenParser Char st (S.Set Flag)
getFlagSet = do 
    getString "["
    result <- sepBy getWord (comma)
    getString "]"
    return (S.fromList result)

getMap :: GenParser Char st a -> GenParser Char st (Map String a)
getMap getBody = do
    getString "["
    list <- sepBy (getMapElement getBody) (comma)
    getString "]"
    return $ fromList list

getMapElement :: GenParser Char st a -> GenParser Char st (String, a)
getMapElement getBody = do
    getString "("
    id <- getWord
    comma
    body <-getBody
    getString ")"
    return (id,body)

getActionBody :: GenParser Char st Action
getActionBody = do 
    getString "{"
    instructions <- many getInstruction
    getString "}"
    return instructions

getInstruction :: GenParser Char st Instruction
getInstruction = getPrint <|> getChange <|> getIf

getPrint :: GenParser Char st Instruction
getPrint = do 
    getString "print"
    info <- getInfo
    return (Print info)

getChange :: GenParser Char st Instruction
getChange = do
    getWhites
    prop <- getProperty
    getWhites
    change <- getChangeType
    getWhites
    value <- getValue
    return (Change prop change value)

getChangeType:: GenParser Char st ChangeType
getChangeType = getWord >>= (\w-> case w of
        "=" -> return Assign
        "+=" -> return Add
        "-=" -> return Delete)

getProperty :: GenParser Char st [String]
getProperty = sepBy getWord (char '.')

getValue :: GenParser Char st ChangeValue
getValue = (getWord >>= return . StringValue)
    <|> (getInfo >>= return . StringValue) 
    <|> (getLocationBody >>= return . LocationValue) 
    <|> (getObjectBody >>= return . ObjectValue)

getIf :: GenParser Char st Instruction
getIf = do 
    getString "if"
    exp <- getExp
    action <- getActionBody
    return (IfStatement exp action)

getExp :: GenParser Char st Exp
getExp = getLeaf <|> getNotExp <|> getBinaryExp

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

