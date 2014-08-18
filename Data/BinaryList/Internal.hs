
-- | This module only contains the 'BinList' data type, with 'Show' and
--   'Functor' instances.
module Data.BinaryList.Internal
  ( BinList (..)
    ) where

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


