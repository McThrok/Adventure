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
    gameFlags::[Flag] -- include Won and Lost
}


data Command = Go | Take | Use | Look | Interact | Inventory | Save | Load | New | Quit | Help

-- data Action = Add | Delete | Assign

-- data Property = Info | Interact | Use | ObjectFlags |
--      Description | Moves | Obects | LocationFlags | 
--      Locations | Current | Backpack | GameFlags | Id String

-- data Value = Object Object | Location Location | Movement Dircetion LocationId | Flag Flag | UseAction UseAction | InterAction InterAction
    
type Direction = String
type ObjectId = String
type LocationId = String
type Flag = String
type UseAction = String
-- type InterAction = (ObjectId, String)
type InterAction = String

type GameStateT a = StateT GameData IO a
