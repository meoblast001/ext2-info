{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Integrity.Superblock
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- Functions for checking the integrity of 'Superblock's.
module Data.EXT2.Integrity.Superblock
( superblockConsistency
, superblockCopiesConsistency
) where

import Data.EXT2.Info.Types
import Data.EXT2.Superblock

-- | Ensure consistency of two 'Superblock's.
--
-- This is given for free by 'Superblock'\'s 'Eq' instance.
superblockConsistency :: Superblock -> Superblock -> IntegrityStatus EXT2Error
superblockConsistency x ((== x) -> True) = Right ()
superblockConsistency _ _ = Left InconsistentSuperblocks
{-# INLINE superblockConsistency #-}

-- | Given a primary superblock and superblock copies, ensures that the
-- superblocks are consistent with the primary superblock.
superblockCopiesConsistency :: Superblock -> [Superblock] ->
                               IntegrityStatus EXT2Error
superblockCopiesConsistency primary = mapM_ (superblockConsistency primary)
{-# INLINE superblockCopiesConsistency #-}
