module Serialize
where


import Data.Map.Lazy
import Data.Binary
import Data.Binary.Derive
import Data.ByteString.Lazy.Internal


data Command = Go|Take|Quit deriving(Eq,Show)

commandDict:: Map String String
commandDict = fromList [("go","Go"),("take","Take"),("quit","Quit")]

serialize :: ByteString
serialize = encode (toList commandDict)

deserialize ::[(String,String)]
deserialize  = decode serialize



