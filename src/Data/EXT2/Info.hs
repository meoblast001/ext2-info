{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Info ( ext2Info ) where

import Control.Monad
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Info.Types (EXT2Error(..))
import Data.EXT2.Inode
import Data.EXT2.Superblock
import Data.EXT2.UsageBitmaps
import System.IO

ext2Info :: Handle -> IO ()
ext2Info handle = do
  superblock <- fetchSuperblock handle
  either (putStrLn . show) (printSuperblockInfo handle) superblock

printSuperblockInfo :: Handle -> Superblock -> IO ()
printSuperblockInfo handle superblock = do
  putStrLn "Superblock is:"
  putStrLn $ show superblock
  bgdTable <- fetchBGDT superblock handle
  zipWithM_ (printBGDInfo handle superblock) bgdTable [0..]

printBGDInfo :: Handle -> Superblock -> BlockGroupDescriptor -> Integer -> IO ()
printBGDInfo handle superblock bgd num = do
  putStrLn ("Block Group Descriptor " ++ show num)
  putStrLn $ show bgd
  (blockUsage, inodeUsage) <- fetchUsageBitmaps superblock bgd handle
  putStrLn (" - Block Usage Bitmap: " ++ show blockUsage)
  putStrLn (" - Inode Usage Bitmap: " ++ show inodeUsage)
  inodeTable <- fetchInodeTable superblock bgd handle
  putStrLn " - Inode Table:"
  mapM_ (\inode -> putStrLn ("   - " ++ show inode)) inodeTable