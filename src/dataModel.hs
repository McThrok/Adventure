module DataModel
where

import Control.Monad.Trans.State
import Data.List
import Data.Map.Lazy hiding (foldl,map)

import DataId

data Object = Object{
    info::String,
    canBeTaken::Bool,
    interact::InterAction,
    use::UseAction
}

data Location = Location{
    description::String,
    moves::Map Direction LocationId,
    objects::Map ObjectId Object
}

data GameResult = Win|Lose|InProgress

data GameData = GameData{
    locations::Map LocationId Location,
    current::LocationId,
    backpack::Map ObjectId Object,
    gameResult::GameResult
}

-- newtype GameState  a = GameState { runState :: GameData -> (GameData, a) }
type GameStateT a = StateT GameData IO a
