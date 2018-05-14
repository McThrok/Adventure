module Parser where
import Text.ParserCombinators.Parsec
import Data.Map.Lazy hiding (foldl,map)

import DataModel

parseAdventureFile :: String -> Either ParseError GameData
parseAdventureFile input = parse adventureFile "(unknown)" input

adventureFile :: GenParser Char st GameData
adventureFile = 
    do
        locations <- getLocations
        string ","
        current <- getCurrent
        string ","
        backpack <- getBackpack
        string ","
        actions <- getActionsMap
        getString ","
        flags <- getFlags
        eof
        return (GameData locations current backpack actions flags)


--locations
getLocations :: GenParser Char st (Map LocationId Location)
getLocations = getString "locations:" >> getMap getLocationBody

getLocationBody :: GenParser Char st Location
getLocationBody = do
    getString "("
    description <- getInfo
    getString ","
    moves <- getMovesMap
    getString ","
    objects <- getObjectsMap
    getString ","
    flags <- getFlagList
    return (Location description moves objects flags)

getMovesMap :: GenParser Char st (Map Direction LocationId )
getMovesMap = getMap getWord

--current
getCurrent :: GenParser Char st LocationId
getCurrent = do 
    getString "current:"
    result <- getWord
    return result


--backpack
getBackpack :: GenParser Char st (Map ObjectId Object) 
getBackpack = do
    getString "backpack:"
    getObjectsMap


--flags
getFlags :: GenParser Char st [String]
getFlags = do 
    getString "flags:"
    result <- getFlagList
    return result
    

--object helpers
getObjectsMap :: GenParser Char st (Map ObjectId Object) 
getObjectsMap = getMap getObjectBody

getObjectBody :: GenParser Char st Object
getObjectBody = do
    getString "("
    info <- getInfo
    getString ","
    interAction <- getWord
    getString ","
    useAction <- getWord
    getString ","
    flags <- getFlagList
    return (Object info interAction useAction flags)

--actions

getActionsMap :: GenParser Char st (Map ActionId Action) 
getActionsMap =  getMap getActionBody

getActionBody :: GenParser Char st Action
getActionBody = return ""

--helpers

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

getInfo ::  GenParser Char st String
getInfo = do
    string "\""
    content <- many (noneOf "\"") 
    string "\""
    return ("\"" ++ content ++ "\"")

getFlagList :: GenParser Char st [String]
getFlagList = sepBy getWord (getString ",")


getMap :: GenParser Char st a -> GenParser Char st (Map String a)
getMap getBody = do
    getString "["
    list <- sepBy (getMapElement getBody) (getString ",")
    getString "]"
    return $ fromList list

getMapElement :: GenParser Char st a -> GenParser Char st (String, a)
getMapElement getBody = do
    getString "("
    id <- getWord
    getString ","
    body <-getBody
    getString ")"
    return (id,body)