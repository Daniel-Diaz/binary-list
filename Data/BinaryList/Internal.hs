
-- | This module only contains the 'BinList' data type, with 'Show' and
--   'Functor' instances, plus the 'fromListBuilder' function.
module Data.BinaryList.Internal
  ( BinList (..)
  , fromListBuilder
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

-- | /O(n)/. This function builds a binary list from a linked list, assuming
--   the length of the input list is a power of two.
fromListBuilder :: [a] -- ^ Input list
                -> Int -- ^ Length index of the input list
                -> BinList a
fromListBuilder [x] _ = ListEnd x
fromListBuilder xs  n =
  let m = n - 1 -- Length index of a single branch
      (l,r) = splitAt (2^m) xs
  in  ListNode n (fromListBuilder l m) (fromListBuilder r m)
