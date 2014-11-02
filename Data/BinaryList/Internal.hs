
-- | This module only contains the 'BinList' data type.
module Data.BinaryList.Internal
  ( BinList (..)
    ) where

import Control.DeepSeq (NFData (..))

-- | A binary list is a list containing a power of two elements.
--   Note that a binary list is never empty.
data BinList a =
        -- Single element list.
        ListEnd a
        -- Given ListNode n l r:
        --   * n >= 1.
        --   * Both l and r have 2^(n-1) elements.
      | ListNode {-# UNPACK #-} !Int (BinList a) (BinList a)
        deriving Eq

instance NFData a => NFData (BinList a) where
  rnf (ListEnd x) = rnf x
  rnf (ListNode _ l r) = rnf l `seq` rnf r
