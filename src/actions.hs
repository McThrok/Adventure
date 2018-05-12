module Actions
where 

import DataId
import DataModel
import Data.Map.Lazy hiding (foldl,map)
import Control.Monad.Trans.State

noneInterAction :: GameStateT ()
noneInterAction = return ()

noneUseAction :: ObjectId-> GameStateT ()
noneUseAction obj = return ()

openChest :: ObjectId->GameStateT ()
openChest Key = get >>= (\s -> put (GameData (locations s) (current s) (newBackpack s) (gameResult s)))
    where 
        newBackpack = (delete Key) . backpack
        newLocations locations = (adjust (\l -> (Location (description l) (moves l) (newObjects(objects l))))) (Bedroom) locations
        newObjects objects = adjust getChest Chest objects
        getChest _ = Object "Chest is open but is empty :(" False NoneInterAction NoneUseAction 


useActions::Map UseAction (ObjectId->GameStateT ())
useActions = fromList [(NoneUseAction, noneUseAction),
    (OpenChest, openChest)]

interActions::Map InterAction (GameStateT ())
interActions = fromList [(NoneInterAction, noneInterAction)]
















