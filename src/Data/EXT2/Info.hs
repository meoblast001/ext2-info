{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Info ( ext2Info ) where

import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Superblock
import System.IO

ext2Info :: Handle -> IO ()
ext2Info handle = do
  superblock <- fetchSuperblock handle
  putStrLn "Superblock is:"
  putStrLn $ show superblock
  bgdTable <- fetchBGDT superblock handle
  mapM_ (\(num, bgd) -> putStrLn ("Block Group Descriptor " ++ show num) >>
                        putStrLn (show bgd)) $ zip [0..] bgdTable
