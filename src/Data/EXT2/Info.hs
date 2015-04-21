-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Info
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- This module contains functions and types for dealing with ext2\'s concept of
-- inodes.
module Data.EXT2.Info ( ext2Info ) where

import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Directory
import Data.EXT2.Superblock
import qualified Data.Vector as V
import System.IO

ext2Info :: Handle -> IO ()
ext2Info handle = do
  superblock <- fetchSuperblock handle
  either print (printSuperblockInfo handle) superblock

printSuperblockInfo :: Handle -> Superblock -> IO ()
printSuperblockInfo handle superblock = do
  putStrLn "Superblock is:"
  print superblock
  bgdTable <- fetchBGDT superblock handle
  V.zipWithM_ (printBGDInfo handle superblock) bgdTable (V.enumFromN 0 (V.length bgdTable))
  printRootDir handle superblock bgdTable

printBGDInfo :: Handle -> Superblock -> BlockGroupDescriptor -> Integer -> IO ()
printBGDInfo _ _ bgd num = do
  putStrLn ("Block Group Descriptor " ++ show num)
  print bgd

printRootDir :: Handle -> Superblock -> V.Vector BlockGroupDescriptor -> IO ()
printRootDir handle superblock bgdTable = do
  tree <- buildFsTree handle superblock bgdTable
  case tree of
    Just fsTree -> putStrLn (show fsTree)
    Nothing -> error "Could not walk filesystem."
