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
  deriving (Eq, Show)

makeLensesWith namespaceLensRules ''EXT2Info

data EXT2Error = GeneralError | InvalidMagicNumber | InconsistentSuperblocks |
                 InconsistentBGDT | UnreachableUsedInode |
                 ReachableUnusedInode | DirectoryInodeUnused |
                 UsedDataBlockWithoutInode | UnusedDataBlockWithInode |
                 InvalidFileCount | InvalidDirectoryCount deriving (Eq, Show)

type IntegrityStatus a = Either a ()
