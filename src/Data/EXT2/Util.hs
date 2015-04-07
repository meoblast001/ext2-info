{-
Copyright (C) 2015 Braden Walters
This file is licensed under the MIT Expat License. See LICENSE.txt.
-}

module Data.EXT2.Util ( createTime ) where

import Data.UnixTime
import Foreign.C.Types

createTime :: Integral a => a -> UnixTime
createTime seconds = UnixTime (CTime $ fromIntegral seconds) 0
