{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Info.Types
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- Types used to expose ext2 information in a pretty way.
module Data.EXT2.Info.Types
( ByteAmount
, EXT2Info(..)
, EXT2Error(..)
, IntegrityStatus

-- * EXT2Info lenses.
, totalSize, usedFileSpaceSize, unusedFileSpaceSize, numInodes, numFiles
, numDirectories, numBlockGroups, blockSize, stateClean
) where

import Control.Lens
import Data.EXT2.Internal.LensHacks

type ByteAmount = Integer

data EXT2Info =
  EXT2Info
  { ext2TotalSize :: ByteAmount
  , ext2UsedFileSpaceSize :: ByteAmount
  , ext2UnusedFileSpaceSize :: ByteAmount
  , ext2NumInodes :: Integer
  , ext2NumFiles :: Integer
  , ext2NumDirectories :: Integer
  , ext2NumBlockGroups :: Integer
  , ext2BlockSize :: ByteAmount
  , ext2StateClean :: Bool }
  deriving (Eq)

makeLensesWith namespaceLensRules ''EXT2Info

instance Show EXT2Info where
  show info =
    "Total size:              " ++ show (info ^. totalSize) ++ "\n" ++
    "Used file space size:    " ++ show (info ^. usedFileSpaceSize) ++ "\n" ++
    "Unused file space size:  " ++ show (info ^. unusedFileSpaceSize) ++ "\n" ++
    "Number of inodes:        " ++ show (info ^. numInodes) ++ "\n" ++
    "Number of files:         " ++ show (info ^. numFiles) ++ "\n" ++
    "Number of directories:   " ++ show (info ^. numDirectories) ++ "\n" ++
    "Number of block groups:  " ++ show (info ^. numBlockGroups) ++ "\n" ++
    "Block size:              " ++ show (info ^. blockSize) ++ "\n" ++
    "Clean:                   " ++ show (info ^. stateClean) ++ "\n"

data EXT2Error = GeneralError | InvalidMagicNumber | InconsistentSuperblocks |
                 InconsistentBGDT | UnreachableUsedInode |
                 ReachableUnusedInode | DirectoryInodeUnused |
                 UsedDataBlockWithoutInode | UnusedDataBlockWithInode |
                 InvalidFileCount | InvalidDirectoryCount deriving (Eq, Show)

type IntegrityStatus a = Either a ()
