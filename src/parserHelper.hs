{-# OPTIONS_HADDOCK hide #-}
module ParserHelper where

import Text.ParserCombinators.Parsec
import Data.Map.Lazy hiding (foldl,map)
import qualified Data.Set as S

import DataModel


getWhites :: GenParser Char st String
getWhites = many (oneOf "\n\t ")

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

comma :: GenParser Char st String
comma = getString ","

getInfo ::  GenParser Char st String
getInfo = do
    string "\""
    content <- many (noneOf "\"") 
    string "\""
    getWhites
    return content

getProperty :: GenParser Char st [String]
getProperty = do
    prop <- sepBy (many1 (alphaNum <|> (char '_'))) (char '.')
    getWhites
    return prop

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

