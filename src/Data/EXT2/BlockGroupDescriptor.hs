{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
( DescriptorNumber
, BlockGroupDescriptor(..)
, BlockGroupDescriptorTable
, fetchBGDT

-- * 'BlockGroupDescriptor' Lenses
, reserve, pad, numDirectories, numUnallocInodes, numUnallocBlocks
, inodeTblStartAddr, inodeUsageAddr, blockUsageAddr, groupNumber
) where

import Control.Applicative
import Control.Lens
import Data.Binary.Get
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.Internal.LensHacks
import Data.EXT2.Superblock
import qualified Data.Vector as V
import System.IO

type DescriptorNumber = Integer

data BlockGroupDescriptor =
  BlockGroupDescriptor
  { bgdGroupNumber :: DescriptorNumber
  , bgdBlockUsageAddr :: Integer
  , bgdInodeUsageAddr :: Integer
  , bgdInodeTblStartAddr :: Integer
  , bgdNumUnallocBlocks :: Integer
  , bgdNumUnallocInodes :: Integer
  , bgdNumDirectories :: Integer
  , bgdPad :: SBS.ByteString
  , bgdReserve :: SBS.ByteString }
  deriving (Eq, Show)

makeLensesWith namespaceLensRules ''BlockGroupDescriptor

type BlockGroupDescriptorTable = V.Vector BlockGroupDescriptor

lenBlockGroupDescriptor :: Integral a => a
lenBlockGroupDescriptor = 32
{-# INLINE lenBlockGroupDescriptor #-}

fetchBGDT :: Superblock -> Handle -> IO BlockGroupDescriptorTable
fetchBGDT superblock handle = do
  let bSize = superblock ^. logBlockSize
      bgdtLoc = if bSize == 1024 then bSize * 2 else bSize
      bgdtSize =
        fromIntegral (numBlockGroups superblock * lenBlockGroupDescriptor)
  hSeek handle AbsoluteSeek bgdtLoc
  runGet (getBGDT $ numBlockGroups superblock) <$> LBS.hGet handle bgdtSize

getBGDT :: Integer -> Get BlockGroupDescriptorTable
getBGDT blockGroups =
  V.mapM getBlockGroupDescriptor (V.enumFromTo 1 blockGroups)
{-# INLINE getBGDT #-}

getBlockGroupDescriptor :: DescriptorNumber -> Get BlockGroupDescriptor
getBlockGroupDescriptor descNum = do
  let getInt = toInteger <$> getWord32le
      getShort = toInteger <$> getWord16le
  BlockGroupDescriptor descNum <$> getInt <*> getInt <*> getInt <*> getShort
                       <*> getShort <*> getShort <*> getByteString 2
                       <*> getByteString 12
