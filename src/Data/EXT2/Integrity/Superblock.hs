{-# LANGUAGE TemplateHaskell #-}
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
( superblockMagicCheck
) where

import Data.EXT2.Info.Types
import Data.EXT2.Superblock

-- | Given a superblock ensure that its magic number is as-expected.
superblockMagicCheck :: Superblock -> IntegrityStatus EXT2Error
superblockMagicCheck (checkIdent -> Left e) = Inconsistent e
superblockMagicCheck _ = Consistent
