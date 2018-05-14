module Parser where
import Text.ParserCombinators.Parsec
import Data.Map.Lazy hiding (foldl,map)

import DataModel

parseAdventureFile :: String -> Either ParseError GameData
parseAdventureFile input = parse adventureFile "(unknown)" input

adventureFile :: GenParser Char st GameData
adventureFile = do
    locations <- getString "locations:" >> getMap getLocationBody
    comma
    current <- getString "current:" >> getWord
    comma
    backpack <- getString "backpack:" >> getObjectsMap
    comma
    actions <- getString "actions:" >> getMap getActionBody
    comma
    useActions <- getString "use actions:" >> getMap getUseActionBody
    comma
    flags <- getString "flags:" >> getFlagList
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
    flags <- getFlagList
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

getActionBody :: GenParser Char st Action
getActionBody = return ""

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
    flags <- getFlagList
    getString ")"
    return (Object info interAction useAction flags)

getWord :: GenParser Char st String
getWord = many (alphaNum <|> (char '_')) 


getString :: String-> GenParser Char st String
getString [] = return ""
getString (c:cs) = do
    optional (char '\n')
    char c
    optional (char '\n')
    tail <- getString cs
    return (c:tail)

comma :: GenParser Char st String
comma = getString ","

getInfo ::  GenParser Char st String
getInfo = do
    string "\""
    content <- many (noneOf "\"") 
    string "\""
    return ("\"" ++ content ++ "\"")

getFlagList :: GenParser Char st [String]
getFlagList = sepBy getWord (comma)

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