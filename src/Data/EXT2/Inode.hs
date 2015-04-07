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
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.EXT2.BlockGroupDescriptor
import Data.EXT2.Superblock
import Data.EXT2.Util (createTime)
import Data.UnixTime
import System.IO

data Inode =
  Inode
  { mode :: Integer
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
  inode <- Inode <$> getShort <*> getShort <*> getInt <*> getTime <*> getTime
                 <*> getTime <*> getTime <*> getShort <*> getShort <*> getShort
                 <*> getInt <*> getByteString 4 <*> replicateM 15 getInt
                 <*> getInt <*> getInt <*> getInt <*> getInt
                 <*> getByteString 12
  (inode:) <$> (getInodeTable (remaining - 1))
  where getInt = toInteger <$> getWord32le
        getShort = toInteger <$> getWord16le
        getTime = createTime <$> (fromIntegral <$> getWord32le)
