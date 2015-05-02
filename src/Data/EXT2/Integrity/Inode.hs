{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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
module Data.EXT2.Integrity.Inode ( reachableInodesUsed ) where

import Control.Lens
import Data.EXT2.Directory
import Data.EXT2.Info.Types
import Data.EXT2.Inode
import Data.EXT2.UsageBitmaps
import qualified Data.Vector as V

reachableInodesUsed :: InodeUsageBitmap -> FsItem -> Either EXT2Error ()
reachableInodesUsed usageBitmap@(InodeUsageBitmap usageBits)
                    (FsDirectory _ inode' children') =
  maybe (Left ReachableUnusedInode)
        (\usage -> if usage
                   then mapM_ (reachableInodesUsed usageBitmap) children'
                   else Left ReachableUnusedInode)
        (usageBits V.!? fromIntegral (inode' ^. inodeNumber - 1))
reachableInodesUsed (InodeUsageBitmap usageBits) (FsFile _ inode') =
  maybe (Left ReachableUnusedInode)
        (\usage -> if usage then Right () else Left ReachableUnusedInode)
        (usageBits V.!? fromIntegral (inode' ^. inodeNumber - 1))
