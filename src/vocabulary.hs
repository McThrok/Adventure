module Vocabulary (vocCommand)
where

import Data.Map.Lazy (Map, fromList)

import DataModel

-- | map of strings to commands
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

