{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
, FsItem(..)
, fetchDirectory
, buildFsTree

-- * Directory lenses.
, inodeRef, recordLen, fileType, name
-- * FsItem lenses.
{-, name-}, inode, childItems
) where

import Control.Lens
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Inode
import Data.EXT2.Internal.LensHacks
import Data.EXT2.Superblock
import Data.Functor
import qualified Data.Traversable as T
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

data FsItem =
  FsDirectory
  { fsitemName :: String
  , fsitemInode :: Inode
  , fsitemChildItems :: [FsItem] } |
  FsFile
  { fsitemName :: String
  , fsitemInode :: Inode }
  deriving (Eq)

makeLensesWith namespaceLensRules ''FsItem

recurShowFsItem :: Int -> FsItem -> String
recurShowFsItem indent (FsDirectory name' _ children') =
  concat (replicate indent "  ") ++ " - " ++ name' ++ " (DIR)\n" ++
  concatMap (recurShowFsItem (indent + 1)) children'
recurShowFsItem indent (FsFile name' _) =
  concat (replicate indent "  ") ++ " - " ++ name' ++ " (FILE)\n"

instance Show FsItem where
  show = recurShowFsItem 0

fetchDirectory :: Handle -> Superblock -> Inode -> IO Directory
fetchDirectory handle sb inode' =
  getDirectory <$> fetchInodeBlocks handle sb inode'
{-# INLINE fetchDirectory #-}

getDirectory :: LBS.ByteString -> Directory
getDirectory bytestring =
  let entry = runGet getDirectoryEntry bytestring
      advanceLen = entry ^. recordLen . to fromIntegral
  in if (entry ^. inodeRef) /= 0 && LBS.length bytestring - advanceLen > 0
     then entry : getDirectory (LBS.drop advanceLen bytestring)
     else [entry]

getDirectoryEntry :: Get DirectoryEntry
getDirectoryEntry = do
  inodeRef' <- toInteger <$> getWord32le
  recordLen' <- toInteger <$> getWord16le
  nameLen <- toInteger <$> getWord8
  fileType' <- toInteger <$> getWord8
  name' <- LBS8.unpack <$> getLazyByteString (fromIntegral nameLen)
  return $ DirectoryEntry inodeRef' recordLen' fileType' name'

buildFsTree :: Handle -> Superblock -> BlockGroupDescriptorTable ->
               IO (Maybe FsItem)
buildFsTree handle sb bgdTable = do
  rootInode <- fetchInode sb bgdTable handle 2
  case rootInode of
    (Just root) -> buildFsTreeRecur handle sb bgdTable "" root
    Nothing -> return Nothing

buildFsTreeRecur :: Handle -> Superblock -> BlockGroupDescriptorTable ->
                    String -> Inode -> IO (Maybe FsItem)
buildFsTreeRecur handle sb bgdTable itemName inode'
  | DirectoryInode `elem` inode' ^. mode = do
    directory <- filter (\entry -> (entry ^. name) `notElem` [".", ".."]) <$>
                 fetchDirectory handle sb inode'
    let childInodeRefs = map dirInodeRef directory
        childNames = map dirName directory
    childInodes <- sequence <$> mapM (fetchInode sb bgdTable handle)
                                     childInodeRefs
    -- This composition of functions may be a bit complex. If childInodes is
    -- Nothing, childItems will be Nothing. childItems will also be Nothing if
    -- any recursive call returns Nothing. Else it will contain a list of child
    -- FsItems.
    childItems' <- join <$> fmap sequence <$> T.sequence (zipWithM
      (buildFsTreeRecur handle sb bgdTable)
      childNames <$> childInodes)
    return (FsDirectory itemName inode' <$> childItems')
  | otherwise = return (Just $ FsFile itemName inode')
