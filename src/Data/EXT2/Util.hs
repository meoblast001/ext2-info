-----------------------------------------------------------------------------
-- |
-- Module      : Data.EXT2.Util
-- Copyright   : (C) 2015 Braden Walters,
--                   2015 Ricky Elrod
-- License     : MIT (see LICENSE file)
-- Maintainer  : Braden Walters <vc@braden-walters.info>,
--               Ricky Elrod <ricky@elrod.me>
-- Stability   : experimental
-- Portability : ghc
--
-- Utility functions used in other modules.
module Data.EXT2.Util ( createTime ) where

import Data.UnixTime
import Foreign.C.Types

createTime :: Integral a => a -> UnixTime
createTime seconds = UnixTime (CTime $ fromIntegral seconds) 0
