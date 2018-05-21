module Main where

import qualified Data.Text as T
import Data.List
import Data.Map.Lazy hiding (foldl,map)
import System.IO
import System.IO.Error
import Prelude hiding (Word)
import Data.Binary hiding (get)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.State.Lazy
import Control.Monad.State (lift)
import Data.Maybe (isJust, fromJust)

import DataModel
import Parser
import Vocabulary
import Game

main :: IO ()
main = mainMenu
