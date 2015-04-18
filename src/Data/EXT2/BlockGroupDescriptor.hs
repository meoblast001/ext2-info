{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.BlockGroupDescriptor
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- This module contains functions and types for dealing with ext2\'s block group
-- descriptor tables.
module Data.EXT2.BlockGroupDescriptor
( BlockGroupDescriptor(..)
, fetchBGDT

-- * 'BlockGroupDescriptor' Lenses
, reserve, pad, numDirectories, numUnallocInodes, numUnallocBlocks
, inodeTblStartAddr, inodeUsageAddr, blockUsageAddr
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.Internal.LensHacks
import Data.EXT2.Superblock
import System.IO

data BlockGroupDescriptor =
  BlockGroupDescriptor
  { bgdBlockUsageAddr :: Integer
  , bgdInodeUsageAddr :: Integer
  , bgdInodeTblStartAddr :: Integer
  , bgdNumUnallocBlocks :: Integer
  , bgdNumUnallocInodes :: Integer
  , bgdNumDirectories :: Integer
  , bgdPad :: SBS.ByteString
  , bgdReserve :: SBS.ByteString }
  deriving (Eq, Show)

makeLensesWith namespaceLensRules ''BlockGroupDescriptor

lenBlockGroupDescriptor :: Integral a => a
lenBlockGroupDescriptor = 32

fetchBGDT :: Superblock -> Handle -> IO [BlockGroupDescriptor]
fetchBGDT superblock handle = do
  let bSize = superblock ^. logBlockSize
      bgdtLoc = if bSize == 1024 then bSize * 2 else bSize
      bgdtSize =
        fromIntegral (numBlockGroups superblock * lenBlockGroupDescriptor)
  hSeek handle AbsoluteSeek bgdtLoc
  runGet (getBGDT $ numBlockGroups superblock) <$> LBS.hGet handle bgdtSize

getBGDT :: Integer -> Get [BlockGroupDescriptor]
getBGDT blockGroups =
  replicateM (fromInteger blockGroups) getBlockGroupDescriptor

getBlockGroupDescriptor :: Get BlockGroupDescriptor
getBlockGroupDescriptor = do
  let getInt = toInteger <$> getWord32le
      getShort = toInteger <$> getWord16le
  BlockGroupDescriptor <$> getInt <*> getInt <*> getInt <*> getShort
                       <*> getShort <*> getShort <*> getByteString 2
                       <*> getByteString 12
