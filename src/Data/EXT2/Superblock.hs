{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Superblock
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- This module exposes functions and types for dealing with ext2 superblocks.
module Data.EXT2.Superblock
( FileSystemState(..)
, ErrorHandlingMethod(..)
, OperatingSystem(..)
, Superblock(..)
, fetchSuperblock
, fetchSuperblockCopies
, numBlockGroups
, blockOffset
, checkIdent
, fileSystemSize
, freeFileSystemSize

-- * 'Superblock' Lenses
, wTime, state, revLevel, rBlocksCount, mntCount, minorRevLevel, maxMntCount
, magic, mTime, logFragSize, logBlockSize, lastCheck, inodesPerGroup
, inodesCount, freeInodesCount, freeBlocksCount, fragsPerGroup, firstDataBlock
, errors, defResuid, defResgid, creatorOs, checkInterval, blocksPerGroup
, blocksCount
) where

import Control.Applicative
import Control.Lens
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.Info.Types (EXT2Error(..))
import Data.EXT2.Internal.LensHacks
import Data.EXT2.Internal.Util (createTime)
import Data.UnixTime
import System.IO

data FileSystemState = StateClean | StateErrors | StateUnknown
  deriving (Eq, Show)
data ErrorHandlingMethod =
  MethodIgnore | MethodRemount | MethodPanic | MethodUnknown deriving (Eq, Show)
data OperatingSystem = Linux | HURD | MASIX | FreeBSD | OSOther | OSUnknown
  deriving (Eq, Show)

data Superblock =
  Superblock {
    sbInodesCount :: Integer
    -- ^ Total number of inodes in the file system
  , sbBlocksCount :: Integer
    -- ^ Total number of blocks in the file system
  , sbRBlocksCount :: Integer
    -- ^ Number of blocks reserved for the superuser
  , sbFreeBlocksCount :: Integer
    -- ^ Number of unallocated blocks
  , sbFreeInodesCount :: Integer
    -- ^ Number of unallocated inodes
  , sbFirstDataBlock :: Integer
    -- ^ Block number of the block containing the superblock
  , sbLogBlockSize :: Integer
    -- ^ log2(block size) - 10
  , sbLogFragSize :: Integer
    -- ^ log2(fragment size) - 10
  , sbBlocksPerGroup :: Integer
    -- ^ Number of blocks in each group
  , sbFragsPerGroup :: Integer
    -- ^ Number of gragments in each group
  , sbInodesPerGroup :: Integer
    -- ^ Number of inodes in each group
  , sbMTime :: UnixTime
    -- ^ Last mount time
  , sbWTime :: UnixTime
    -- ^ Last write time
  , sbMntCount :: Integer
    -- ^ Number of mounts since last consistency check
  , sbMaxMntCount :: Integer
    -- ^ Number of allowed mounts before requiring a consistency check
  , sbMagic :: Integer
    -- ^ ext2 signature: @0xef53@
  , sbState :: FileSystemState
    -- ^ Filesystem state
  , sbErrors :: ErrorHandlingMethod
    -- ^ What to do on an error condition
  , sbMinorRevLevel :: Integer
    -- ^ Minor portion of version
  , sbLastCheck :: UnixTime
    -- ^ Time of last consistency check
  , sbCheckInterval :: UnixTime
    -- ^ Interval between forced consistency checks
  , sbCreatorOs :: OperatingSystem
    -- ^ Operating system ID
  , sbRevLevel :: Integer
    -- ^ Major portion of version
  , sbDefResuid :: Integer
    -- ^ User ID that can use reserved blocks
  , sbDefResgid :: Integer
    -- ^ Group ID that can use reserved blocks
  } deriving (Eq, Show)

makeLensesWith namespaceLensRules ''Superblock

type SuperblockCopies = [Superblock]

lenSuperblock :: Integral a => a
lenSuperblock = 1024
{-# INLINE lenSuperblock #-}

fetchSuperblock :: Handle -> IO (Either EXT2Error Superblock)
fetchSuperblock handle = do
  hSeek handle AbsoluteSeek 1024
  checkIdent <$> runGet getSuperblock <$> LBS.hGet handle lenSuperblock

fetchSuperblockCopies :: Handle -> Superblock ->
                         IO (Either EXT2Error SuperblockCopies)
fetchSuperblockCopies handle sb = do
  first <- fetchOneCopy 1
  -- Returns a list if IO when an IO containing a list is needed. Therefore
  -- sequence is used.
  rest <- sequence $ recurFetchSparseCopies 1 3 5 7
  -- Similarly, Either a [b] needed instead of [Either a b].
  return $ sequence (first:rest)
  where recurFetchSparseCopies bgNum nextPow3 nextPow5 nextPow7
          | bgNum >= numBlockGroups sb = []
          | bgNum == nextPow3 =
              (fetchOneCopy bgNum):(recurFetchSparseCopies (bgNum + 1)
                                   (nextPow3 * 3) nextPow5 nextPow7)
          | bgNum == nextPow5 =
              (fetchOneCopy bgNum):(recurFetchSparseCopies (bgNum + 1)
                                   nextPow3 (nextPow5 * 5) nextPow7)
          | bgNum == nextPow7 =
              (fetchOneCopy bgNum):(recurFetchSparseCopies (bgNum + 1)
                                   nextPow3 nextPow5 (nextPow7 * 7))
          | otherwise =
              recurFetchSparseCopies (bgNum + 1) nextPow3 nextPow5 nextPow7
        fetchOneCopy bgNum = do
          let startPos = blockOffset sb (bgNum * sb ^. blocksPerGroup + 1)
          hSeek handle AbsoluteSeek startPos
          checkIdent <$> runGet getSuperblock <$> LBS.hGet handle lenSuperblock

getSuperblock :: Get Superblock
getSuperblock =
  Superblock <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt
             <*> ((\x -> 2 ^ (x + 10)) <$> getInt)
             <*> ((\x -> 2 ^ (x + 10)) <$> getInt)
             <*> getInt <*> getInt <*> getInt
             <*> getTime <*> getTime
             <*> getShort <*> getShort <*> getShort
             <*> (getFsState <$> getShort)
             <*> (getErrorHandlingMethod <$> getShort)
             <*> getShort <*> getTime <*> getTime
             <*> (getOS <$> getInt) <*> getInt <*> getShort <*> getShort
  where getInt = toInteger <$> getWord32le
        getShort = toInteger <$> getWord16le
        getTime = createTime <$> (fromIntegral <$> getWord32le :: Get Integer)

checkIdent :: Superblock -> Either EXT2Error Superblock
checkIdent superblock
  | superblock ^. magic == 0xef53 = Right superblock
  | otherwise = Left InvalidMagicNumber
{-# INLINE checkIdent #-}

getFsState :: Integer -> FileSystemState
getFsState 1 = StateClean
getFsState 2 = StateErrors
getFsState _ = StateUnknown
{-# INLINE getFsState #-}

getErrorHandlingMethod :: Integer -> ErrorHandlingMethod
getErrorHandlingMethod 1 = MethodIgnore
getErrorHandlingMethod 2 = MethodRemount
getErrorHandlingMethod 3 = MethodPanic
getErrorHandlingMethod _ = MethodUnknown
{-# INLINE getErrorHandlingMethod #-}

getOS :: Integer -> OperatingSystem
getOS 0 = Linux
getOS 1 = HURD
getOS 2 = MASIX
getOS 3 = FreeBSD
getOS 4 = OSOther
getOS _ = OSUnknown
{-# INLINE getOS #-}

numBlockGroups :: Superblock -> Integer
numBlockGroups superblock =
  let numBlocksF = superblock ^. blocksCount . to fromIntegral
      numBlocksPerGroupF = superblock ^. blocksPerGroup . to fromIntegral
  in ceiling (numBlocksF / numBlocksPerGroupF :: Double)
{-# INLINE numBlockGroups #-}

blockOffset :: Superblock -> Integer -> Integer
blockOffset sb block = 1024 + (block - 1) * sb ^. logBlockSize
{-# INLINE blockOffset #-}

fileSystemSize :: Superblock -> Integer
fileSystemSize sb =
  sb ^. to numBlockGroups * sb ^. blocksPerGroup * sb ^. logBlockSize
{-# INLINE fileSystemSize #-}

freeFileSystemSize :: Superblock -> Integer
freeFileSystemSize sb =
  sb ^. freeBlocksCount * sb ^. logBlockSize
{-# INLINE freeFileSystemSize #-}
