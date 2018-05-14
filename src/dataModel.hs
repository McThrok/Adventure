module DataModel
where

import Control.Monad.Trans.State
import Data.List
import Data.Map.Lazy hiding (foldl,map)

data Object = Object{
    info::String,
    interact::InterAction,
    use::UseAction,
    objectFlags::[Flag] -- includes canBeTaken
}

data Location = Location{
    description::String,
    moves::Map Direction LocationId,
    objects::Map ObjectId Object,
    locationFlags::[Flag]
}

data GameData = GameData{
    locations::Map LocationId Location,
    current::LocationId,
    backpack::Map ObjectId Object,
    actions::Map ActionId Action,
    gameFlags::[Flag] -- include Won and Lost
}


data Command = Go | Take | Use | Look | Interact | Inventory | Save | Load | New | Quit | Help

-- data Action = Add | Delete | Assign

-- data Property = Info | Interact | Use | ObjectFlags |
--      Description | Moves | Obects | LocationFlags | 
--      Locations | Current | Backpack | GameFlags | Id String

-- data Value = Object Object | Location Location | Movement Dircetion LocationId | Flag Flag | UseAction UseAction | InterAction InterAction

type Action = String

type Direction = String
type ObjectId = String
type ActionId = String
type LocationId = String
type Flag = String
type UseAction = ActionId
-- type InterAction = (ActionId, ObjectId) -- Action ObjectId
type InterAction = ActionId

type GameStateT a = StateT GameData IO a
