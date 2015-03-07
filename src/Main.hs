{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Main where

import Data.EXT2.Info
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> withFile fileName ReadMode ext2Info
    _ -> error "Please specify a file name."
