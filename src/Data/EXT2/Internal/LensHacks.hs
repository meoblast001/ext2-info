-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Internal.LensHacks
-- Copyright   : (C) 2014 Ricky Elrod
-- License     : BSD2 (see LICENSE file)
-- Maintainer  : Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : lens, template-haskell
----------------------------------------------------------------------------
module Data.EXT2.Internal.LensHacks where

import Data.Char
import Data.List as List
import Data.Maybe
import Control.Lens
import Control.Lens.Internal.FieldTH
import Language.Haskell.TH

namespaceLensRules :: LensRules
namespaceLensRules = defaultFieldRules { _fieldToDef = abbreviatedNamer }
{-# INLINE namespaceLensRules #-}

-- | This is taken straight out of 'Control.Lens.TH' but modified to give
-- a 'TopName' back instead of a 'MethodName'. This means we can
-- 'makeLensesWith'out classes using abbreviated fields.

abbreviatedNamer :: Name -> [Name] -> Name -> [DefName]
abbreviatedNamer _ fields field = maybeToList $ do
  fieldPart <- stripMaxLc (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  stripMaxLc f = do x <- stripPrefix optUnderscore f
                    case break isUpper x of
                      (p,s) | List.null p || List.null s -> Nothing
                            | otherwise                  -> Just s
  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing
