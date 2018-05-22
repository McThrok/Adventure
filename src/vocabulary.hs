module Vocabulary
where

import Data.Map.Lazy hiding (foldl,map)
import Prelude hiding (Word)

import DataModel

vocCommand :: Map String Command
vocCommand = fromList [
    ("go", Go),
    ("take",  Take),
    ("use", Use),
    ("look", Look),
    ("interact", Interact),
    ("inventory", Inventory),
    ("save", Save),
    ("load", Load),
    ("new", New),
    ("quit", Quit),
    ("help", Help)]

