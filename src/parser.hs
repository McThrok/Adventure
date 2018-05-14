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
        flags <- getFlags
        eof
        return (GameData locations current backpack flags)


--locations
getLocations :: GenParser Char st (Map LocationId Location)
getLocations = do
    getString "locations:["
    locations <- sepBy getLocation (getString ",")
    getString "]"
    return (fromList locations)

getLocation ::GenParser Char st (LocationId, Location)
getLocation = do
    getString "("
    id <- getWord
    getString ","
    body <-getLocationBody
    getString ")"
    return (id,body)
    

getLocationBody :: GenParser Char st Location
getLocationBody = do
    getString "("
    description <- getInfo
    getString ","
    moves <- getMovesMap
    objects <- getObjectsMap
    getString ","
    getString ","
    flags <- getFlagList
    return (Location description moves objects flags)

getMovesMap :: GenParser Char st (Map Direction LocationId )
getMovesMap = do
    getString "["
    locations <- sepBy getMove (getString ",")
    getString "]"
    return (fromList locations)
        
getMove ::GenParser Char st (Direction, LocationId)
getMove = do
    getString "("
    direction <- getWord
    getString ","
    locationId <- getWord
    getString ")"
    return (direction, locationId)


--current
getCurrent :: GenParser Char st LocationId
getCurrent = do 
    getString "current:"
    result <-  getWord
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
getObjectsMap = do
    getString "["
    objects <- sepBy getObject (getString ",")
    getString "]"
    return (fromList objects)

getObject :: GenParser Char st (ObjectId, Object) 
getObject = do
    getString "("
    id <- getWord
    getString ","
    body <-getObjectBody
    getString ")"
    return (id,body)

getObjectBody :: GenParser Char st Object
getObjectBody = do
    getString "("
    info <- getInfo
    getString ","
    interAction <- getAction
    getString ","
    useAction <- getAction
    getString ","
    flags <- getFlagList
    return (Object info interAction useAction flags)

getAction :: GenParser Char st String
getAction = return ""


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