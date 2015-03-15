{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Superblock
( FileSystemState(..)
, ErrorHandlingMethod(..)
, OperatingSystem(..)
, Superblock(..)
, fetchSuperblock
, numBlockGroups
) where

import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.UnixTime
import Foreign.C.Types
import System.IO

data FileSystemState = StateClean | StateErrors deriving (Show)
data ErrorHandlingMethod = MethodIgnore | MethodRemount | MethodPanic
  deriving (Show)
data OperatingSystem = Linux | HURD | MASIX | FreeBSD | Other deriving (Show)

data Superblock =
  Superblock
  { numINodes :: Integer
  , numBlocks :: Integer
  , numSuReservedBlocks :: Integer
  , numUnallocBlocks :: Integer
  , numUnallocINodes :: Integer
  , superBlockNum :: Integer
  , blockSize :: Integer
  , fragmentSize :: Integer
  , numBlocksPerGroup :: Integer
  , numFragmentsPerGroup :: Integer
  , numINodesPerGroup :: Integer
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
  deriving (Show)

fetchSuperblock :: Handle -> IO Superblock
fetchSuperblock handle = do
  hSeek handle AbsoluteSeek 1024
  checkIdent <$> runGet getSuperblock <$> LBS.hGetContents handle

getSuperblock :: Get Superblock
getSuperblock = do
  let getInt = toInteger <$> getWord32le
      getShort = toInteger <$> getWord16le
      createTime seconds = UnixTime (CTime seconds) 0
  Superblock <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt
             <*> ((\x -> 2 ^ (x + 10)) <$> getInt)
             <*> ((\x -> 2 ^ (x + 10)) <$> getInt)
             <*> getInt <*> getInt <*> getInt
             <*> (createTime <$> (fromIntegral <$> getWord32le))
             <*> (createTime <$> (fromIntegral <$> getWord32le))
             <*> getShort <*> getShort <*> getShort
             <*> (getFsState <$> getShort)
             <*> (getErrorHandlingMethod <$> getShort)
             <*> getShort
             <*> (createTime <$> (fromIntegral <$> getWord32le))
             <*> (createTime <$> (fromIntegral <$> getWord32le))
             <*> (getOS <$> getInt) <*> getInt <*> getShort <*> getShort

checkIdent :: Superblock -> Superblock
checkIdent superblock
  | signature superblock == 0xef53 = superblock
  | otherwise = error ("Invalid identifier " ++ show (signature superblock))

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
