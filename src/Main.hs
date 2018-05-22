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
main = putStrLn "qwe\nasd\nzxc" >> mainMenu

-- main :: IO ()
-- main = runStateT doSth 1 >>= putStrLn . show

-- doSth :: MyStateT Integer
-- doSth = Control.Monad.Trans.State.Lazy.put 11 >> qwe 321>> get

-- qwe :: Integer -> MyStateT Integer
-- qwe a = Control.Monad.Trans.State.Lazy.modify (+ a) >> return 1

-- type MyStateT a = StateT Integer IO a
