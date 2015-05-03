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
module Data.EXT2.Info
( ext2Info
, ext2Debug
) where

import Control.Lens
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Directory
import Data.EXT2.Info.Types
import Data.EXT2.Integrity.BlockGroupDescriptor
import Data.EXT2.Integrity.Inode
import Data.EXT2.Integrity.Superblock
import Data.EXT2.Superblock as Superblock
import Data.EXT2.UsageBitmaps
import Data.Functor
import qualified Data.Vector as V
import System.IO

ext2Info :: Handle -> IO (Either EXT2Error EXT2Info)
ext2Info handle = do
  superblockOrErr <- fetchSuperblock handle
  case superblockOrErr of
    Left err -> return $ Left err
    Right superblock -> do
      superblockCopiesOrErr <- fetchSuperblockCopies handle superblock
      case superblockCopiesOrErr of
        Left err -> return $ Left err
        Right superblockCopies -> do
          bgdTable <- fetchBGDT superblock handle
          bgdTableCopies <- fetchBGDTCopies superblock superblockCopies handle
          usageBitmaps <- fetchUsageBitmaps superblock bgdTable handle
          fsTreeMay <- buildFsTree handle superblock bgdTable
          case fsTreeMay of
            Just fsTree ->
              maybe (Right <$> generateInfo handle superblock bgdTable fsTree)
                    (return . Left)
                    (checkConsistency superblock superblockCopies bgdTable
                                      bgdTableCopies usageBitmaps fsTree)
            Nothing -> return $ Left GeneralError

checkConsistency :: Superblock -> SuperblockCopies ->
                    BlockGroupDescriptorTable ->
                    BlockGroupDescriptorTableCopies ->
                    (BlockUsageBitmap, InodeUsageBitmap) -> FsItem ->
                    Maybe EXT2Error
checkConsistency sb sbCopies bgdTable bgdTableCopies (_, inodeUsage) fsTree =
  either Just (const Nothing) doCheck
  where doCheck = do
          superblockCopiesConsistency sb sbCopies
          bgdTableCopiesConsistency bgdTable bgdTableCopies
          usedInodesReachable inodeUsage fsTree
          reachableInodesUsed inodeUsage fsTree

generateInfo :: Handle -> Superblock -> BlockGroupDescriptorTable -> FsItem ->
                IO EXT2Info
generateInfo _ sb bgdTable fsRoot = do
  return EXT2Info {
      ext2TotalSize = sb ^. to fileSystemSize,
      ext2UsedFileSpaceSize = sb ^. to fileSystemSize -
                              sb ^. to freeFileSystemSize,
      ext2UnusedFileSpaceSize = sb ^. to freeFileSystemSize,
      ext2NumInodes = sb ^. inodesCount,
      ext2NumFiles = countFiles fsRoot,
      ext2NumDirectories = V.foldl (+) 0 (V.map bgdNumDirectories bgdTable),
      ext2NumBlockGroups = Superblock.numBlockGroups sb,
      ext2BlockSize = sb ^. logBlockSize,
      ext2StateClean = sb ^. state == StateClean
    }

ext2Debug :: Handle -> IO ()
ext2Debug handle = do
  superblockOrErr <- fetchSuperblock handle
  case superblockOrErr of
    Left _ -> error "Superblock failure."
    Right superblock -> printSuperblockInfo handle superblock

printSuperblockInfo :: Handle -> Superblock -> IO ()
printSuperblockInfo handle superblock = do
  putStrLn "Superblock is:"
  print superblock
  bgdTable <- fetchBGDT superblock handle
  V.zipWithM_ (printBGDInfo handle superblock) bgdTable (V.enumFromN 0 (V.length bgdTable))
  printSuperblockCopyInfo handle superblock
  printRootDir handle superblock bgdTable

printSuperblockCopyInfo :: Handle -> Superblock -> IO ()
printSuperblockCopyInfo handle superblock = do
  putStrLn "Superblock copies are:"
  superblocks <- fetchSuperblockCopies handle superblock
  case superblocks of
    Left ext2error -> print ext2error
    Right copies -> do
      mapM_ print copies
      bgdTableCopies <- fetchBGDTCopies superblock copies handle
      mapM_ (\tbl -> V.zipWithM_ (printBGDInfo handle superblock) tbl
                                 (V.enumFromN 0 (V.length tbl))) bgdTableCopies

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
