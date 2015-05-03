-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Integrity.Inode
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- Functions for checking the integrity of 'Inode' and in some cases
-- 'Directory'.
module Data.EXT2.Integrity.Inode
( usedInodesReachable
, reachableInodesUsed
) where

import Control.Lens
import Data.EXT2.Directory
import Data.EXT2.Info.Types
import Data.EXT2.Inode
import Data.EXT2.UsageBitmaps
import Data.List ((\\))
import Data.Maybe
import qualified Data.Vector as V

usedInodesReachable :: InodeUsageBitmap -> FsItem -> IntegrityStatus EXT2Error
usedInodesReachable usageBitmap fsTree =
  let reachNums =  V.fromList $ reachableInodeNumbers fsTree
      -- Inodes 1 - 10 are reserved in EXT2.
      usedInodeNums = usedInodeNumbers usageBitmap \\ [1..10]
      good = all (`V.elem` reachNums) usedInodeNums
  in if good then Right () else Left UnreachableUsedInode

reachableInodesUsed :: InodeUsageBitmap -> FsItem -> IntegrityStatus EXT2Error
reachableInodesUsed (InodeUsageBitmap usageBits) fsTree =
  let reachNums = reachableInodeNumbers fsTree
      good = all
             (\num -> fromMaybe False $ usageBits V.!? fromIntegral (num - 1))
             reachNums
  in if good then Right () else Left ReachableUnusedInode

reachableInodeNumbers :: FsItem -> [InodeNumber]
reachableInodeNumbers (FsDirectory _ inode' children') =
  (inode' ^. inodeNumber) : concatMap reachableInodeNumbers children'
reachableInodeNumbers (FsFile _ inode') = [inode' ^. inodeNumber]

usedInodeNumbers :: InodeUsageBitmap -> [InodeNumber]
usedInodeNumbers (InodeUsageBitmap usageBits) =
  V.toList $ V.filter (/= 0) $ V.imap
    (\idx bit -> if bit then fromIntegral idx + 1 else 0) usageBits
