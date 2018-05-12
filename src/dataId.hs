module DataId
where

data Command = Go | Take | Use | On | Look | Interact | Inventory | Save | Load | New | Quit | Help
    
data Direction = North | East | Upstairs | In | Out | Forward

data ObjectId = Key | Chest | Lamp deriving (Eq,Show,Ord)
    
data LocationId = Kitchen|Bedroom deriving (Eq,Show,Ord)

data UseAction = OpenChest | NoneUseAction deriving (Eq,Show,Ord)

data InterAction = SwitchLamp | NoneInterAction deriving (Eq,Show,Ord)

data Word = Command Command | Direction Direction | ObjectId ObjectId
