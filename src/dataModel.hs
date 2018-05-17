module DataModel
where

import Control.Monad.Trans.State
import Data.List
import Data.Set
import Data.Map.Lazy hiding (foldl,map)


data Command = Go | Take | Use | Look | Interact | Inventory | Save | Load | New | Quit | Help

type Direction = String
type ObjectId = String
type LocationId = String
type ActionId = String
type UseActionId = String

type Flag = String

data Object = Object{
    info::String,
    interact::ActionId,
    use::ActionId,
    objectFlags::Set Flag -- includes canBeTaken
}

data Location = Location{
    description::String,
    moves::Map Direction LocationId,
    objects::Map ObjectId Object,
    locationFlags::Set Flag
}

data GameData = GameData{
    locations::Map LocationId Location,
    current::LocationId,
    backpack::Map ObjectId Object,
    interActions::Map ActionId Action,
    useActions::Map ActionId (ObjectId, Action),
    gameFlags::Set Flag -- include Won and Lost
}

type GameStateT a = StateT GameData IO a

type Action = [Instruction]
data Instruction = Print String | Change [String] ChangeType ChangeValue | IfStatement Exp Action
data ChangeType = Add | Delete | Assign
data ChangeValue =  StringValue String | ObjectValue Object | LocationValue Location
data Exp = Leaf [String] | Not Exp | And Exp Exp | Or Exp Exp

