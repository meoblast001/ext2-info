{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Inode
( InodeMode(..)
, Inode(..)
, fetchInodeTable
, usedInodes
) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Superblock
import Data.EXT2.UsageBitmaps
import Data.EXT2.Util (createTime)
import Data.Maybe
import Data.UnixTime
import System.IO

data InodeMode = -- File Format.
                 SocketInode | SymLinkInode | RegFileInode | BlockDevInode |
                 DirectoryInode | CharDevInode | FifoInode deriving (Show)

intToFileFormatMode :: Integer -> Maybe InodeMode
intToFileFormatMode 0xc000 = Just SocketInode
intToFileFormatMode 0xa000 = Just SymLinkInode
intToFileFormatMode 0x8000 = Just RegFileInode
intToFileFormatMode 0x6000 = Just BlockDevInode
intToFileFormatMode 0x4000 = Just DirectoryInode
intToFileFormatMode 0x2000 = Just CharDevInode
intToFileFormatMode 0x1000 = Just FifoInode
intToFileFormatMode _ = Nothing

data Inode =
  Inode
  { mode :: [InodeMode]
  , userId :: Integer
  , size :: Integer
  , accessTime :: UnixTime
  , creationTime :: UnixTime
  , modifiedTime :: UnixTime
  , deletedTime :: UnixTime
  , groupId :: Integer
  , linkCount :: Integer
  , blocks512 :: Integer
  , flags :: Integer
  , osDependantValue :: SBS.ByteString
  , directBlocks :: [Integer]
  , indirectBlocks :: (Integer, Integer, Integer)
  , generation :: Integer
  , fileAcl :: Integer
  , dirAcl :: Integer
  , faddr :: Integer
  , osDependantValue2 :: SBS.ByteString }
  deriving (Show)

fetchInodeTable :: Superblock -> BlockGroupDescriptor -> Handle -> IO [Inode]
fetchInodeTable sb bgd handle = do
  hSeek handle AbsoluteSeek $ blockOffset sb $ inodeTblStartAddr bgd
  runGet (getInodeTable $ numInodesPerGroup sb) <$> LBS.hGetContents handle

getInodeTable :: Integer -> Get [Inode]
getInodeTable 0 = return []
getInodeTable remaining = do
  inode <- Inode <$> maybeToList <$> (intToFileFormatMode <$> getShort)
                 <*> getShort <*> getInt <*> getTime <*> getTime <*> getTime
                 <*> getTime <*> getShort <*> getShort <*> getShort <*> getInt
                 <*> getByteString 4 <*> replicateM 12 getInt
                 <*> liftA3 (,,) getInt getInt getInt <*> getInt <*> getInt
                 <*> getInt <*> getInt <*> getByteString 12
  (inode:) <$> (getInodeTable (remaining - 1))
  where getInt = toInteger <$> getWord32le
        getShort = toInteger <$> getWord16le
        getTime = createTime <$> (fromIntegral <$> getWord32le)

usedInodes :: InodeUsageBitmap -> [Inode] -> [Inode]
usedInodes inodeUsage allInodes =
  map fst $ filter snd $ zip allInodes $ inodeUsageBool inodeUsage
