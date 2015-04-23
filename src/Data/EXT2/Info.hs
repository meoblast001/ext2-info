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

import Control.Lens
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Directory
import Data.EXT2.Info.Types
import Data.EXT2.Superblock as Superblock
import Data.Functor
import qualified Data.Vector as V
import System.IO

ext2Info :: Handle -> IO (Either EXT2Error EXT2Info)
ext2Info handle = do
  superblockOrErr <- fetchSuperblock handle
  case superblockOrErr of
    Left err -> return $ Left err
    Right superblock -> do
      -- Begin debug code.
      printSuperblockInfo handle superblock
      -- End debug code.
      bgdTable <- fetchBGDT superblock handle
      fsTreeMay <- buildFsTree handle superblock bgdTable
      case fsTreeMay of
        Just fsTree ->
          Right <$> generateInfo handle superblock bgdTable fsTree
        Nothing -> return $ Left GeneralError

generateInfo :: Handle -> Superblock -> BlockGroupDescriptorTable -> FsItem ->
                IO EXT2Info
generateInfo handle sb bgdTable fsRoot = do
  totalSize' <- hFileSize handle
  return EXT2Info {
      ext2TotalSize = totalSize',
      ext2UsedFileSpaceSize = 0, -- To be completed.
      ext2UnusedFileSpaceSize = 0, -- To be completed.
      ext2SpaceUsed = 0, -- To be completed.
      ext2NumInodes = 0, -- To be completed.
      ext2NumFiles = countFiles fsRoot,
      ext2NumDirectories = V.foldl (+) 0 (V.map bgdNumDirectories bgdTable),
      ext2NumBlockGroups = Superblock.numBlockGroups sb,
      ext2BlockSize = sb ^. logBlockSize,
      ext2StateClean = sb ^. state == StateClean
    }

countFiles :: FsItem -> Integer
countFiles dir@(FsDirectory {}) = sum $ map countFiles (dir ^. childItems)
countFiles (FsFile {}) = 1

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

printRootDir :: Handle -> Superblock -> BlockGroupDescriptorTable -> IO ()
printRootDir handle superblock bgdTable = do
  tree <- buildFsTree handle superblock bgdTable
  case tree of
    Just fsTree -> print fsTree
    Nothing -> error "Could not walk filesystem."
