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
, blockUsageBool
, inodeUsageBool
, fetchUsageBitmaps
) where

import Control.Lens
import Control.Monad
import Data.Binary.Get
import Data.Bits.Bitwise (toListBE)
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Superblock
import Data.Functor
import Data.Word
import System.IO

data BlockUsageBitmap = BlockUsageBitmap Integer [Word8] deriving (Eq)
data InodeUsageBitmap = InodeUsageBitmap Integer [Word8] deriving (Eq)

lenUsageBitmaps :: Integral a => Superblock -> a
lenUsageBitmaps sb = sb ^. logBlockSize . to fromIntegral

instance Show BlockUsageBitmap where
  show bm@(BlockUsageBitmap len _) =
    concatMap (\bool -> if bool then "1" else "0") (blockUsageBool bm) ++
    "(Len: " ++ show len ++ ")"

instance Show InodeUsageBitmap where
  show bm@(InodeUsageBitmap len _) =
    concatMap (\bool -> if bool then "1" else "0") (inodeUsageBool bm) ++
    "(Len: " ++ show len ++ ")"

blockUsageBool :: BlockUsageBitmap -> [Bool]
blockUsageBool (BlockUsageBitmap len words') =
  take (fromIntegral len) $ concatMap toListBE words'

inodeUsageBool :: InodeUsageBitmap -> [Bool]
inodeUsageBool (InodeUsageBitmap len words') =
  take (fromIntegral len) $ concatMap toListBE words'

fetchUsageBitmaps :: Superblock -> BlockGroupDescriptor -> Handle ->
                     IO (BlockUsageBitmap, InodeUsageBitmap)
fetchUsageBitmaps sb bgd handle = do
  hSeek handle AbsoluteSeek $ blockOffset sb $ blockUsageAddr bgd
  blockUsage <- runGet (getBlockUsageBitmap sb) <$>
                LBS.hGet handle (lenUsageBitmaps sb)
  hSeek handle AbsoluteSeek $ blockOffset sb $ inodeUsageAddr bgd
  inodeUsage <- runGet (getInodeUsageBitmap sb) <$>
                LBS.hGet handle (lenUsageBitmaps sb)
  return (blockUsage, inodeUsage)

getBlockUsageBitmap :: Superblock -> Get BlockUsageBitmap
getBlockUsageBitmap sb =
  BlockUsageBitmap (sb ^. blocksCount) <$>
    replicateM (sb ^. blocksCount . to fromIntegral) getWord8

getInodeUsageBitmap :: Superblock -> Get InodeUsageBitmap
getInodeUsageBitmap sb =
  InodeUsageBitmap (sb ^. inodesCount) <$>
    replicateM (sb ^. blocksCount . to fromIntegral) getWord8
