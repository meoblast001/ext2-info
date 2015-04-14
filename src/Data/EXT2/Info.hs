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

import Control.Monad
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Info.Types (EXT2Error(..))
import Data.EXT2.Inode
import Data.EXT2.Superblock
import Data.EXT2.UsageBitmaps
import Data.Functor
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
  inodeTable <- usedInodes inodeUsage <$> fetchInodeTable superblock bgd handle
  putStrLn " - Inode Table:"
  mapM_ (\inode -> putStrLn ("   - " ++ show inode)) inodeTable
