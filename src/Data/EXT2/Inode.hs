{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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
, fetchInode
, usedInodes
, fetchInodeBlocks
, fetchDataBlockNumbers

-- * 'Inode' Lenses
, mode, size, userId, accessTime, creationTime, modifiedTime, deletedTime
, groupId, linkCount, blocks512, flags, osDependentValue, generation, fileAcl
, dirAcl, faddr, osDependentValue2
) where

import Control.Applicative
import Control.Lens
import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Internal.LensHacks
import Data.EXT2.Superblock
import Data.EXT2.UsageBitmaps
import Data.EXT2.Internal.Util (createTime)
import Data.Maybe
import Data.UnixTime
import qualified Data.Vector as V
import System.IO

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
{-# INLINE intToFileFormatMode #-}

data Inode =
  Inode
  { inoMode :: [InodeMode]
  , inoUserId :: Integer
  , inoSize :: Integer
  , inoAccessTime :: UnixTime
  , inoCreationTime :: UnixTime
  , inoModifiedTime :: UnixTime
  , inoDeletedTime :: UnixTime
  , inoGroupId :: Integer
  , inoLinkCount :: Integer
  , inoBlocks512 :: Integer
  , inoFlags :: Integer
  , inoOsDependentValue :: SBS.ByteString
  , inoDirectBlocks :: [Integer]
  , inoIndirectBlocks :: (Integer, Integer, Integer)
  , inoGeneration :: Integer
  , inoFileAcl :: Integer
  , inoDirAcl :: Integer
  , inoFaddr :: Integer
  , inoOsDependentValue2 :: SBS.ByteString }
  deriving (Eq, Show)

makeLensesWith namespaceLensRules ''Inode

lenInode :: Integral a => a
lenInode = 128
{-# INLINE lenInode #-}

fetchInodeTable :: Superblock -> BlockGroupDescriptor -> Handle -> IO [Inode]
fetchInodeTable sb bgd handle = do
  hSeek handle AbsoluteSeek $ blockOffset sb $ bgd ^. inodeTblStartAddr
  replicateM (sb ^. inodesPerGroup . to fromIntegral)
    (runGet getInode <$> LBS.hGet handle lenInode)

fetchInode :: Superblock -> V.Vector BlockGroupDescriptor -> Handle -> Integer ->
              IO (Maybe Inode)
fetchInode sb bgdTable handle inodeNum =
  let groupIndex = (inodeNum - 1) `quot` sb ^. inodesPerGroup
      localInodeNum = (inodeNum - 1) - (groupIndex * sb ^. inodesPerGroup)
  in case bgdTable V.!? fromIntegral groupIndex of
    Just bgd -> do
      let inodeLoc = blockOffset sb (bgd ^. inodeTblStartAddr) +
                     (lenInode * localInodeNum)
      hSeek handle AbsoluteSeek inodeLoc
      Just <$> runGet getInode <$> LBS.hGet handle lenInode
    Nothing -> return Nothing

getInode :: Get Inode
getInode =
  Inode <$> maybeToList <$> (intToFileFormatMode <$> getShort)
        <*> getShort <*> getInt <*> getTime <*> getTime <*> getTime
        <*> getTime <*> getShort <*> getShort <*> getInt <*> getInt
        <*> getByteString 4 <*> replicateM 12 getInt
        <*> liftA3 (,,) getInt getInt getInt <*> getInt <*> getInt
        <*> getInt <*> getInt <*> getByteString 12
  where getInt = toInteger <$> getWord32le
        getShort = toInteger <$> getWord16le
        getTime = createTime <$> (fromIntegral <$> getWord32le :: Get Integer)

usedInodes :: InodeUsageBitmap -> [Inode] -> [Inode]
usedInodes inodeUsage allInodes =
  map fst $ filter snd $ zip allInodes $ inodeUsageBool inodeUsage
{-# INLINE usedInodes #-}

fetchInodeBlocks :: Handle -> Superblock -> Inode -> IO LBS.ByteString
fetchInodeBlocks handle sb inode = do
  blockNums <- fetchDataBlockNumbers handle sb inode
  LBS.concat <$> mapM readBlock blockNums
  where readBlock num = do
          hSeek handle AbsoluteSeek $ blockOffset sb (fromIntegral num)
          LBS.hGet handle $ fromIntegral (sb ^. logBlockSize)

fetchDataBlockNumbers :: Handle -> Superblock -> Inode -> IO [Integer]
fetchDataBlockNumbers handle sb inode = do
  let usedDirect = takeWhile (not . (== 0)) (inode ^. directBlocks)
      (indir1Num, indir2Num, indir3Num) = inode ^. indirectBlocks
  indir1 <- fetchIndirectBlock1 handle sb indir1Num
  indir2 <- fetchIndirectBlock2 handle sb indir2Num
  indir3 <- fetchIndirectBlock3 handle sb indir3Num
  return (usedDirect ++ indir1 ++ indir2 ++ indir3)

get32IntBlockTillZero :: Superblock -> Get [Integer]
get32IntBlockTillZero sb =
  let num32Integers =
        floor ((sb ^. logBlockSize . to fromIntegral) / 4 :: Double)
      getInt = toInteger <$> getWord32le
  in fst <$> break (== 0) <$> replicateM num32Integers getInt

fetchIndirectBlock1 :: Handle -> Superblock -> Integer -> IO [Integer]
fetchIndirectBlock1 _ _ 0 = return []
fetchIndirectBlock1 handle sb blockNum = do
  hSeek handle AbsoluteSeek $ blockOffset sb blockNum
  runGet (get32IntBlockTillZero sb) <$>
         LBS.hGet handle (sb ^. logBlockSize . to fromIntegral)

fetchIndirectBlock2 :: Handle -> Superblock -> Integer -> IO [Integer]
fetchIndirectBlock2 _ _ 0 = return []
fetchIndirectBlock2 handle sb blockNum = do
  hSeek handle AbsoluteSeek $ blockOffset sb blockNum
  indirectBlocks' <- runGet (get32IntBlockTillZero sb) <$>
                    LBS.hGet handle (sb ^. logBlockSize . to fromIntegral)
  concat <$> mapM (fetchIndirectBlock1 handle sb) indirectBlocks'

fetchIndirectBlock3 :: Handle -> Superblock -> Integer -> IO [Integer]
fetchIndirectBlock3 _ _ 0 = return []
fetchIndirectBlock3 handle sb blockNum = do
  hSeek handle AbsoluteSeek $ blockOffset sb blockNum
  indirectBlocks' <- runGet (get32IntBlockTillZero sb) <$>
                    LBS.hGet handle (sb ^. logBlockSize . to fromIntegral)
  concat <$> mapM (fetchIndirectBlock2 handle sb) indirectBlocks'
