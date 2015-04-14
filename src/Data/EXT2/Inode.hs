-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Inode
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
module Data.EXT2.Inode
( InodeMode(..)
, Inode(..)
, fetchInodeTable
, usedInodes
, fetchDataBlockNumbers
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

 -- | File Format.
data InodeMode =
  SocketInode | SymLinkInode | RegFileInode | BlockDevInode |
  DirectoryInode | CharDevInode | FifoInode deriving (Eq, Show)

intToFileFormatMode :: Integer -> Maybe InodeMode
intToFileFormatMode input
  | (input .&. 0xf000) == 0xc000 = Just SocketInode
  | (input .&. 0xf000) == 0xa000 = Just SymLinkInode
  | (input .&. 0xf000) == 0x8000 = Just RegFileInode
  | (input .&. 0xf000) == 0x6000 = Just BlockDevInode
  | (input .&. 0xf000) == 0x4000 = Just DirectoryInode
  | (input .&. 0xf000) == 0x2000 = Just CharDevInode
  | (input .&. 0xf000) == 0x1000 = Just FifoInode
  | otherwise = Nothing

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
  deriving (Eq, Show)

lenInode :: Integral a => a
lenInode = 128

fetchInodeTable :: Superblock -> BlockGroupDescriptor -> Handle -> IO [Inode]
fetchInodeTable sb bgd handle = do
  let inodeTableSize = fromIntegral (numInodesPerGroup sb * lenInode)
  hSeek handle AbsoluteSeek $ blockOffset sb $ inodeTblStartAddr bgd
  replicateM (fromIntegral (numInodesPerGroup sb))
             (runGet getInode <$> LBS.hGet handle lenInode)

getInode :: Get Inode
getInode =
  Inode <$> maybeToList <$> (intToFileFormatMode <$> getShort)
        <*> getShort <*> getInt <*> getTime <*> getTime <*> getTime
        <*> getTime <*> getShort <*> getShort <*> getShort <*> getInt
        <*> getByteString 4 <*> replicateM 12 getInt
        <*> liftA3 (,,) getInt getInt getInt <*> getInt <*> getInt
        <*> getInt <*> getInt <*> getByteString 12
  where getInt = toInteger <$> getWord32le
        getShort = toInteger <$> getWord16le
        getTime = createTime <$> (fromIntegral <$> getWord32le)

usedInodes :: InodeUsageBitmap -> [Inode] -> [Inode]
usedInodes inodeUsage allInodes =
  map fst $ filter snd $ zip allInodes $ inodeUsageBool inodeUsage

fetchDataBlockNumbers :: Handle -> Superblock -> Inode -> IO [Integer]
fetchDataBlockNumbers handle sb inode = do
  let usedDirect = takeWhile (not . (== 0)) (directBlocks inode)
      (indir1Num, indir2Num, indir3Num) = indirectBlocks inode
  indir1 <- fetchIndirectBlock1 handle sb indir1Num
  indir2 <- fetchIndirectBlock2 handle sb indir2Num
  indir3 <- fetchIndirectBlock3 handle sb indir3Num
  return (usedDirect ++ indir1 ++ indir2 ++ indir3)

get32IntBlockTillZero :: Superblock -> Get [Integer]
get32IntBlockTillZero sb = do
  let num32Integers = floor (fromIntegral (blockSize sb) / 4)
      getInt = toInteger <$> getWord32le
  fst <$> break (== 0) <$> replicateM num32Integers getInt

fetchIndirectBlock1 :: Handle -> Superblock -> Integer -> IO [Integer]
fetchIndirectBlock1 _ _ 0 = return []
fetchIndirectBlock1 handle sb blockNum = do
  hSeek handle AbsoluteSeek $ blockOffset sb blockNum
  runGet (get32IntBlockTillZero sb) <$>
         LBS.hGet handle (fromInteger $ blockSize sb)

fetchIndirectBlock2 :: Handle -> Superblock -> Integer -> IO [Integer]
fetchIndirectBlock2 _ _ 0 = return []
fetchIndirectBlock2 handle sb blockNum = do
  hSeek handle AbsoluteSeek $ blockOffset sb blockNum
  indirectBlocks <- runGet (get32IntBlockTillZero sb) <$>
                    LBS.hGet handle (fromInteger $ blockSize sb)
  concat <$> mapM (fetchIndirectBlock1 handle sb) indirectBlocks

fetchIndirectBlock3 :: Handle -> Superblock -> Integer -> IO [Integer]
fetchIndirectBlock3 _ _ 0 = return []
fetchIndirectBlock3 handle sb blockNum = do
  hSeek handle AbsoluteSeek $ blockOffset sb blockNum
  indirectBlocks <- runGet (get32IntBlockTillZero sb) <$>
                    LBS.hGet handle (fromInteger $ blockSize sb)
  concat <$> mapM (fetchIndirectBlock2 handle sb) indirectBlocks
