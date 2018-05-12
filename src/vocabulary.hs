module Volcabulary
where

import Data.Map.Lazy hiding (foldl,map)
import Prelude hiding (Word)

import DataId

vocCommand :: Map String Command
vocCommand = fromList [
    ("go", Go),
    ("take",  Take),
    ("use", Use),
    ("on", On),
    ("look", Look),
    ("interact", Interact),
    ("inventory", Inventory),
    ("save", Save),
    ("load", Load),
    ("new", New),
    ("quit", Quit),
    ("help", Help)]

vocObject :: Map String ObjectId
vocObject = fromList [
    ("key", Key),
    ("chest", Chest),
    ("lamp", Lamp)]

vocDirection :: Map String Direction
vocDirection = fromList [
    ("north", North),
    ("east", East),
    ("upstairs", Upstairs),
    ("in", In),
    ("out", Out),
    ("forward", Forward)]

-- volcabulary :: Map String Word
-- -- volcabulary = fromList ((toList vocCommand) ++ (toList vocObject) ++ (toList vocDirection))
-- volcabulary = fromList ((toList vocCommand))