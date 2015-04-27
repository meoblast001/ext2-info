{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Integrity.BlockGroupDescriptor
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- Functions for checking the integrity of 'BlockGroupDescriptor's.
module Data.EXT2.Integrity.BlockGroupDescriptor
( blockGroupDescriptorConsistency
) where

import Data.EXT2.Info.Types
import Data.EXT2.BlockGroupDescriptor

-- | Ensure consistency of two 'BlockGroupDescriptor's.
--
-- This is given for free by 'BlockGroupDescriptor'\'s 'Eq' instance.
blockGroupDescriptorConsistency
  :: BlockGroupDescriptor
  -> BlockGroupDescriptor
  -> IntegrityStatus EXT2Error
blockGroupDescriptorConsistency x ((== x) -> True) = Consistent
blockGroupDescriptorConsistency _ _ = Inconsistent InconsistentSuperblocks
{-# INLINE blockGroupDescriptorConsistency #-}
