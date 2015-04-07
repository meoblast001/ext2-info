{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Inode
( Inode
, fetchInodeTable
) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.Bits.Bitwise as Bitwise
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Superblock
import Data.EXT2.Util (createTime)
import Data.UnixTime
import System.IO

data InodeMode = SocketInode | SymLinkInode | RegFileInode | BlockDevInode |
                 DirectoryInode | CharDevInode | FifoInode deriving (Show)

inodeModes :: [(Integer, InodeMode)]
inodeModes = [
    (0xc000, SocketInode),
    (0xa000, SymLinkInode),
    (0x8000, RegFileInode),
    (0x6000, BlockDevInode),
    (0x4000, DirectoryInode),
    (0x2000, CharDevInode),
    (0x1000, FifoInode)
  ]

intToInodeModes :: Integer -> [InodeMode]
intToInodeModes integer =
  map snd $ filter (\(code, _) -> Bitwise.or (code .&.  integer)) inodeModes

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
  , blocks :: [Integer]
  , generation :: Integer
  , fileAcl :: Integer
  , dirAcl :: Integer
  , faddr :: Integer
  , osDependantValue2 :: SBS.ByteString }
  deriving (Show)

fetchInodeTable :: Superblock -> BlockGroupDescriptor -> Handle -> IO [Inode]
fetchInodeTable sb bgd handle = do
  let tableLoc = blockSize sb * inodeTblStartAddr bgd
  hSeek handle AbsoluteSeek tableLoc
  runGet (getInodeTable $ numInodesPerGroup sb) <$> LBS.hGetContents handle

getInodeTable :: Integer -> Get [Inode]
getInodeTable 0 = return []
getInodeTable remaining = do
  inode <- Inode <$> intToInodeModes <$> (fromIntegral <$> getWord16le)
                 <*> getShort <*> getInt <*> getTime <*> getTime <*> getTime
                 <*> getTime <*> getShort <*> getShort <*> getShort <*> getInt
                 <*> getByteString 4 <*> replicateM 15 getInt <*> getInt
                 <*> getInt <*> getInt <*> getInt <*> getByteString 12
  (inode:) <$> (getInodeTable (remaining - 1))
  where getInt = toInteger <$> getWord32le
        getShort = toInteger <$> getWord16le
        getTime = createTime <$> (fromIntegral <$> getWord32le)
