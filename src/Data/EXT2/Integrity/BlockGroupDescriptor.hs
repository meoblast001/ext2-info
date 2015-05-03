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
, bgdTableConsistency
, bgdTableCopiesConsistency
) where

import Data.EXT2.Info.Types
import Data.EXT2.BlockGroupDescriptor
import qualified Data.Vector as V

-- | Ensure consistency of two 'BlockGroupDescriptor's.
--
-- This is given for free by 'BlockGroupDescriptor'\'s 'Eq' instance.
blockGroupDescriptorConsistency
  :: BlockGroupDescriptor
  -> BlockGroupDescriptor
  -> IntegrityStatus EXT2Error
blockGroupDescriptorConsistency x ((== x) -> True) = Right ()
blockGroupDescriptorConsistency _ _ = Left InconsistentBGDT
{-# INLINE blockGroupDescriptorConsistency #-}

bgdTableConsistency :: BlockGroupDescriptorTable -> BlockGroupDescriptorTable ->
                       IntegrityStatus EXT2Error
bgdTableConsistency lhs rhs =
  if V.length lhs == V.length rhs
    then if V.and (V.zipWith (==) lhs rhs) then Right ()
                                           else Left InconsistentBGDT
    else Left InconsistentBGDT
{-# INLINE bgdTableConsistency #-}

-- | Given a primary superblock and superblock copies, ensures that the
-- superblocks are consistent with the primary superblock.
bgdTableCopiesConsistency :: BlockGroupDescriptorTable ->
                             BlockGroupDescriptorTableCopies ->
                             IntegrityStatus EXT2Error
bgdTableCopiesConsistency primary = mapM_ (bgdTableConsistency primary)
{-# INLINE bgdTableCopiesConsistency #-}
