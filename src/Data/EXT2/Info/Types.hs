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
  deriving (Eq, Show)

data EXT2Error = InvalidMagicNumber | InconsistentSuperblocks |
                 InconsistentBGDT | UnreachableUsedInode |
                 ReachableUnusedInode | DirectoryInodeUnused |
                 UsedDataBlockWithoutInode | UnusedDataBlockWithInode |
                 InvalidFileCount | InvalidDirectoryCount deriving (Eq, Show)
