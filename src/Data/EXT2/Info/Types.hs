{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Info.Types
( ByteAmount
, EXT2Info(..)
, EXT2Error(..)
) where

type ByteAmount = Integer

data EXT2Info =
  EXT2Info
  { totalSize :: ByteAmount
  , usedFileSpaceSize :: ByteAmount
  , unusedFileSpaceSize :: ByteAmount
  , spaceUsed :: ByteAmount
  , numInodes :: Integer
  , numFiles :: Integer
  , numDirectories :: Integer
  , numBlockGroups :: Integer
  , blockSize :: ByteAmount }
  deriving (Show)

data EXT2Error = InvalidMagicNumber | InconsistentSuperblocks |
                 InconsistentBGDT | UnreachableUsedInode |
                 ReachableUnusedInode | DirectoryInodeUnused |
                 UsedDataBlockWithoutInode | UnusedDataBlockWithInode |
                 InvalidFileCount | InvalidDirectoryCount deriving (Show)
