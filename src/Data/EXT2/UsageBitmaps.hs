-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.UsageBitmaps
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
module Data.EXT2.UsageBitmaps
( BlockUsageBitmap(..)
, InodeUsageBitmap(..)
, fetchUsageBitmaps
, fetchPartialUsageBitmaps
) where

import Control.Lens
import Data.Binary.Get
import Data.Bits.Bitwise (toListBE)
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Superblock
import Data.Functor
import qualified Data.Vector as V
import System.IO

newtype BlockUsageBitmap = BlockUsageBitmap (V.Vector Bool) deriving (Eq)
newtype InodeUsageBitmap = InodeUsageBitmap (V.Vector Bool) deriving (Eq)

lenUsageBitmaps :: Integral a => Superblock -> a
lenUsageBitmaps sb = sb ^. logBlockSize . to fromIntegral
{-# INLINE lenUsageBitmaps #-}

instance Show BlockUsageBitmap where
  show (BlockUsageBitmap bits) =
    V.toList (V.concatMap (\bool -> V.fromList (if bool then "1" else "0"))
                          bits)

instance Show InodeUsageBitmap where
  show (InodeUsageBitmap bits) =
    V.toList (V.concatMap (\bool -> V.fromList (if bool then"1" else "0")) bits)

fetchUsageBitmaps :: Superblock -> BlockGroupDescriptorTable -> Handle ->
                     IO (BlockUsageBitmap, InodeUsageBitmap)
fetchUsageBitmaps sb bgdTable handle = do
  (block, inode) <- V.unzip <$> V.mapM
                    (\bgd -> fetchPartialUsageBitmaps sb bgd handle) bgdTable
  let blockBits = V.concatMap (\(BlockUsageBitmap bits) -> bits) block
      inodeBits = V.concatMap (\(InodeUsageBitmap bits) -> bits) inode
  return (BlockUsageBitmap blockBits, InodeUsageBitmap inodeBits)

fetchPartialUsageBitmaps :: Superblock -> BlockGroupDescriptor -> Handle ->
                            IO (BlockUsageBitmap, InodeUsageBitmap)
fetchPartialUsageBitmaps sb bgd handle = do
  hSeek handle AbsoluteSeek $ blockOffset sb $ bgd ^. blockUsageAddr
  blockUsage <- runGet (getBlockUsageBitmap sb) <$>
                LBS.hGet handle (lenUsageBitmaps sb)
  hSeek handle AbsoluteSeek $ blockOffset sb $ bgd ^. inodeUsageAddr
  inodeUsage <- runGet (getInodeUsageBitmap sb) <$>
                LBS.hGet handle (lenUsageBitmaps sb)
  return (blockUsage, inodeUsage)

getBlockUsageBitmap :: Superblock -> Get BlockUsageBitmap
getBlockUsageBitmap sb = do
  words' <- V.replicateM (sb ^. logBlockSize . to fromIntegral) getWord8
  let bits = V.concatMap (V.fromList . toListBE) words'
  return $ BlockUsageBitmap (V.take (sb ^. blocksPerGroup . to fromIntegral)
                                    bits)

getInodeUsageBitmap :: Superblock -> Get InodeUsageBitmap
getInodeUsageBitmap sb = do
  words' <- V.replicateM (sb ^. logBlockSize . to fromIntegral) getWord8
  let bits = V.concatMap (V.fromList . toListBE) words'
  return $ InodeUsageBitmap (V.take (sb ^. inodesPerGroup . to fromIntegral)
                                    bits)
