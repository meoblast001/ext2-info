{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Info.Types
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- Types used to expose ext2 information in a pretty way.
module Data.EXT2.Info.Types
( ByteAmount
, EXT2Info(..)
, EXT2Error(..)
, IntegrityStatus(..)

, maybeIso
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Monoid

type ByteAmount = Integer

data EXT2Info =
  EXT2Info
  { totalSize :: ByteAmount
  , usedFileSpaceSize :: ByteAmount
  , unusedFileSpaceSize :: ByteAmount
  , spaceUsed :: ByteAmount
  , numInodes :: Integer
  , numFiles :: Integer
  , numDirectories :: Integer
  , numBlockGroups :: Integer
  , blockSize :: ByteAmount }
  deriving (Eq, Show)

data EXT2Error = InvalidMagicNumber | InconsistentSuperblocks |
                 InconsistentBGDT | UnreachableUsedInode |
                 ReachableUnusedInode | DirectoryInodeUnused |
                 UsedDataBlockWithoutInode | UnusedDataBlockWithInode |
                 InvalidFileCount | InvalidDirectoryCount deriving (Eq, Show)

-- | 'IntegrityStatus' is isomorphic to Maybe, but typically 'Nothing' is used
-- to show a missing value or an error condition, rather than a success (and
-- we consider being consistent to be a success value).
data IntegrityStatus a = Consistent
                       | Inconsistent a
                       deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative IntegrityStatus where
  pure = Inconsistent
  (<*>) = ap

instance Monad IntegrityStatus where
  return = pure
  Consistent >>= _ = Consistent
  Inconsistent x >>= f = f x

instance Alternative IntegrityStatus where
  empty = Consistent
  Consistent <|> p = p
  incx@(Inconsistent _) <|> _ = incx

instance MonadPlus IntegrityStatus where
  mzero = empty
  Consistent `mplus` ys = ys
  xs `mplus` _ = xs

instance Monoid a => Monoid (IntegrityStatus a) where
  mempty = empty
  Consistent `mappend` m = m
  m `mappend` Consistent = m
  Inconsistent x `mappend` Inconsistent y = Inconsistent (x `mappend` y)

maybeIso :: Iso' (IntegrityStatus a) (Maybe a)
maybeIso = iso toMaybe fromMaybe
  where
    fromMaybe (Just x) = Inconsistent x
    fromMaybe Nothing = Consistent
    toMaybe (Inconsistent x) = Just x
    toMaybe Consistent = Nothing
