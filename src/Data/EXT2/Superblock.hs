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
, numBlockGroups
, blockOffset
) where

import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.Info.Types (EXT2Error(..))
import Data.EXT2.Util (createTime)
import Data.UnixTime
import System.IO

data FileSystemState = StateClean | StateErrors deriving (Eq, Show)
data ErrorHandlingMethod =
  MethodIgnore | MethodRemount | MethodPanic deriving (Eq, Show)
data OperatingSystem = Linux | HURD | MASIX | FreeBSD | Other deriving (Eq, Show)

data Superblock =
  Superblock
  { numInodes :: Integer
  , numBlocks :: Integer
  , numSuReservedBlocks :: Integer
  , numUnallocBlocks :: Integer
  , numUnallocINodes :: Integer
  , superBlockNum :: Integer
  , blockSize :: Integer
  , fragmentSize :: Integer
  , numBlocksPerGroup :: Integer
  , numFragmentsPerGroup :: Integer
  , numInodesPerGroup :: Integer
  , lastMountTime :: UnixTime
  , lastWrittenTime :: UnixTime
  , numMountsSinceFsck :: Integer
  , numMountsForReqFsck :: Integer
  , signature :: Integer
  , fsState :: FileSystemState
  , errorHandlingMethod :: ErrorHandlingMethod
  , minorVersion :: Integer
  , lastFsckTime :: UnixTime
  , intervalReqFsck :: UnixTime
  , creatorOS :: OperatingSystem
  , majorVersion :: Integer
  , superUserID :: Integer
  , superGroupID :: Integer }
  deriving (Eq, Show)

lenSuperblock :: Integral a => a
lenSuperblock = 1024

fetchSuperblock :: Handle -> IO (Either EXT2Error Superblock)
fetchSuperblock handle = do
  hSeek handle AbsoluteSeek 1024
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
        getTime = createTime <$> (fromIntegral <$> getWord32le)

checkIdent :: Superblock -> Either EXT2Error Superblock
checkIdent superblock
  | signature superblock == 0xef53 = Right superblock
  | otherwise = Left InvalidMagicNumber

getFsState :: Integer -> FileSystemState
getFsState 1 = StateClean
getFsState 2 = StateErrors
getFsState _ = error "Unknown filesystem state."

getErrorHandlingMethod :: Integer -> ErrorHandlingMethod
getErrorHandlingMethod 1 = MethodIgnore
getErrorHandlingMethod 2 = MethodRemount
getErrorHandlingMethod 3 = MethodPanic
getErrorHandlingMethod _ = error "Unknown error handling method."

getOS :: Integer -> OperatingSystem
getOS 0 = Linux
getOS 1 = HURD
getOS 2 = MASIX
getOS 3 = FreeBSD
getOS 4 = Other
getOS _ = error "Unknown operating system."

numBlockGroups :: Superblock -> Integer
numBlockGroups superblock =
  let numBlocksF = fromIntegral $ numBlocks superblock
      numBlocksPerGroupF = fromIntegral $ numBlocksPerGroup superblock
  in ceiling (numBlocksF / numBlocksPerGroupF)

blockOffset :: Superblock -> Integer -> Integer
blockOffset sb block = 1024 + (block - 1) * blockSize sb
