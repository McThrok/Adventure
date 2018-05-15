module Parser where
import Text.ParserCombinators.Parsec
import Data.Map.Lazy hiding (foldl,map)
import qualified Data.Set as S

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
    flags <- getFlagSet
    getString ")"
    return (Object info interAction useAction flags)

getWord :: GenParser Char st String
getWord = many (alphaNum <|> (char '_')) 

getString :: String-> GenParser Char st String
getString str = do
    many getWhite
    string str
    many getWhite
    return str

getWhite :: GenParser Char st Char
getWhite = oneOf "\n\t "

comma :: GenParser Char st String
comma = getString ","

getInfo ::  GenParser Char st String
getInfo = do
    string "\""
    content <- many (noneOf "\"") 
    string "\""
    return ("\"" ++ content ++ "\"")

getFlagSet :: GenParser Char st (S.Set Flag)
getFlagSet = sepBy getWord (comma) >>= return . S.fromList

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