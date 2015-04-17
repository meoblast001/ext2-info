{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Directory
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- This module exposes functions and types for dealing with ext2 directories in
-- the filesystem.
module Data.EXT2.Directory
( DirectoryEntry(..)
, fetchDirectory

-- * Directory lenses.
, inodeRef, recordLen, fileType, name
) where

import Control.Lens
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.EXT2.Inode
import Data.EXT2.Internal.LensHacks
import Data.EXT2.Superblock
import Data.Functor
import System.IO

data DirectoryEntry =
  DirectoryEntry
  { dirInodeRef :: Integer
  , dirRecordLen :: Integer
  , dirFileType :: Integer
  , dirName :: String }
  deriving (Eq, Show)

makeLensesWith namespaceLensRules ''DirectoryEntry

type Directory = [DirectoryEntry]

fetchDirectory :: Handle -> Superblock -> Inode -> IO Directory
fetchDirectory handle sb inode = do
  getDirectory <$> fetchInodeBlocks handle sb inode

getDirectory :: LBS.ByteString -> Directory
getDirectory bytestring =
  let entry = runGet getDirectoryEntry bytestring
      advanceLen = entry ^. recordLen . to fromIntegral
  in if (entry ^. inodeRef) /= 0 && (LBS.length bytestring) - advanceLen > 0
     then entry:(getDirectory (LBS.drop advanceLen bytestring))
     else [entry]

getDirectoryEntry :: Get DirectoryEntry
getDirectoryEntry = do
  inodeRef' <- toInteger <$> getWord32le
  recordLen' <- toInteger <$> getWord16le
  nameLen <- toInteger <$> getWord8
  fileType' <- toInteger <$> getWord8
  name' <- LBS8.unpack <$> getLazyByteString (fromIntegral nameLen)
  return $ DirectoryEntry inodeRef' recordLen' fileType' name'
