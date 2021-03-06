{-# LANGUAGE DeriveGeneric #-}
module DataModel
where

import GHC.Generics
import Control.Monad.Trans.State
import Data.List
import Data.Set
import Data.Map.Lazy hiding (foldl,map)
import Data.Binary

-- | commands for all actions
data Command = Go | Take | Use | Look | Interact | Inventory | Save | Load | New | Quit | Help deriving (Generic, Show)

type Direction = String
type ObjectId = String
type LocationId = String
type ActionId = String
type UseActionId = String

type Flag = String

data Object = Object{
    objectInfo::String,
    interAction::ActionId,
    useAction::ActionId,
    objectFlags::Set Flag -- includes canBeTaken
} deriving (Generic, Show)

data Location = Location{
    locationInfo::String,
    moves::Map Direction LocationId,
    objects::Map ObjectId Object,
    locationFlags::Set Flag
} deriving (Generic, Show)

data GameData = GameData{
    locations::Map LocationId Location,
    current::LocationId,
    backpack::Map ObjectId Object,
    interActions::Map ActionId Action,
    useActions::Map ActionId (ObjectId, Action),
    gameFlags::Set Flag -- includes gameFinished
} deriving (Generic, Show)


type GameStateT a = StateT GameData IO a

type Action = [Instruction]
data Instruction = Print String | Change [String] ChangeType ChangeValue | IfStatement Exp Action deriving (Generic, Show)
data ChangeType = Add | Delete | Assign deriving (Generic, Show)
data ChangeValue =  StringValue String | ObjectValue Object | LocationValue Location deriving (Generic, Show)
data Exp = Leaf [String] | Not Exp | And Exp Exp | Or Exp Exp deriving (Generic, Show)


instance Binary GameData
instance Binary Location
instance Binary Object
instance Binary Instruction
instance Binary ChangeType
instance Binary ChangeValue
instance Binary Exp