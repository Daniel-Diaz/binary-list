
-- | Binary lists are lists whose number of elements is a power of two.
--   This data structure is efficient for some computations like:
--
-- * Splitting a list in half.
-- * Appending two lists of the same length.
-- * Extracting an element from the list.
--
--   All the functions exported are total except for 'fromListWithDefault'.
--   It is impossible for the user of this library to create a binary list
--   whose length is /not/ a power of two.
--
--   Since many names in this module crashes with the names of some "Prelude"
--   functions, you probably want to import this module this way:
--
-- > import Data.BinaryList (BinList)
-- > import qualified Data.BinaryList as BL
--
module Data.BinaryList (
    -- * Type
    BinList
    -- * Construction
  , singleton
  , append
  , replicate
    -- * Queries
  , lengthIndex
  , length
  , lookup
  , head
  , last
    -- * Decontruction
  , split
  , fold
    -- * Transformation
  , reverse
    -- * Tuples
  , joinPairs
  , disjoinPairs
    -- * Zipping and Unzipping
  , zip , unzip
  , zipWith
    -- * Lists
  , fromList
  , fromListWithDefault
  , toList
  ) where

import Prelude hiding (length,lookup,replicate,head,last,zip,unzip,zipWith,reverse)
import qualified Prelude
import Data.Bits ((.&.))
import Foreign.Storable (sizeOf)
import Data.List (find)

-- | A binary list is a list containing a power of two elements.
--   Note that a binary list is never empty.
data BinList a =
        -- Single element list.
        ListEnd a
        -- Given ListNode n l r:
        --   * n >= 1.
        --   * Both l and r have 2^(n-1) elements.
      | ListNode Int (BinList a) (BinList a)
        deriving Eq

-- | /O(1)/. Build a list with a single element.
singleton :: a -> BinList a
singleton = ListEnd

-- | /O(1)/. Given a binary list @l@ with length @2^k@:
--
-- > lengthIndex l = k
--
lengthIndex :: BinList a -> Int
lengthIndex (ListNode n _ _) = n
lengthIndex (ListEnd _) = 0

-- | /O(1)/. Number of elements in the list.
length :: BinList a -> Int
length = (2^) . lengthIndex

-- | /O(log n)/. Lookup an element in the list by its index (starting from 0).
--   If the index is out of range, 'Nothing' is returned.
lookup :: BinList a -> Int -> Maybe a
lookup (ListNode n l r) i =
   let m = 2^(n-1) -- Number of elements in a single branch
   in  if i < m
          then lookup l i       -- Lookup in the left branch
          else lookup r $ i - m -- Lookup in the right branch
lookup (ListEnd x) 0 = Just x
lookup _ _ = Nothing

-- | /O(1)/. Append two binary lists. This is only possible
--   if both lists have the same length. If this condition
--   is not hold, 'Nothing' is returned.
append :: BinList a -> BinList a -> Maybe (BinList a)
append xs ys =
  let i = lengthIndex xs
  in  if i == lengthIndex ys
         then Just $ ListNode (i+1) xs ys
         else Nothing

-- | /O(1)/. Split a binary list into two sublists of half the length,
--   unless the list only contains one element. In that case, it
--   just returns that element.
split :: BinList a -> Either a (BinList a,BinList a)
split (ListNode _ l r) = Right (l,r)
split (ListEnd x) = Left x

-- | /O(log n)/. Calling @replicate n x@ builds a binary list with
--   @2^n@ occurences of @x@.
replicate :: Int -> a -> BinList a
replicate 0 x = ListEnd x
replicate n x =
  let b = replicate (n-1) x -- Both branches of the binary list
  in  ListNode n b b -- Note that both branches are the same shared object

-- | Fold a binary list using an operator.
fold :: (a -> a -> a) -> BinList a -> a
fold f (ListNode _ l r) = f (fold f l) (fold f r)
fold _ (ListEnd x) = x

-- | /O(log n)/. Get the first element of a binary list.
head :: BinList a -> a
head (ListNode _ l _) = head l
head (ListEnd x) = x

-- | /O(log n)/. Get the last element of a binary list.
last :: BinList a -> a
last (ListNode _ _ r) = last r
last (ListEnd x) = x

-- | /O(n)/. Reverse a binary list.
reverse :: BinList a -> BinList a
reverse (ListNode n l r) = ListNode n (reverse r) (reverse l)
reverse xs = xs

------------------------------
-- Transformations with tuples

-- | /O(n)/. Transform a list of pairs into a flat list. The
--   resulting list will have twice more elements than the
--   original.
joinPairs :: BinList (a,a) -> BinList a
joinPairs (ListEnd (x,y)) = ListNode 1 (ListEnd x) (ListEnd y)
joinPairs (ListNode n l r) = ListNode (n+1) (joinPairs l) (joinPairs r)

-- | /O(n)/. Opposite transformation of 'joinPairs'. It halves
--   the number of elements of the input. As a result, when
--   applied to a binary list with a single element, it returns
--   'Nothing'.
disjoinPairs :: BinList a -> Maybe (BinList (a,a))
disjoinPairs (ListEnd _) = Nothing
disjoinPairs xs = Just $ disjoinPairsNodes xs

disjoinPairsNodes :: BinList a -> BinList (a,a)
disjoinPairsNodes (ListNode _ (ListEnd x) (ListEnd y)) = ListEnd (x,y)
disjoinPairsNodes (ListNode n l r) = ListNode (n-1) (disjoinPairsNodes l) (disjoinPairsNodes r)
disjoinPairsNodes _ = error "disjoinPairsNodes: bug. Please, report this with an example input."

------------------------
-- Zipping and Unzipping

-- | /O(n)/. Zip two binary lists using an operator.
zipWith :: (a -> b -> c) -> BinList a -> BinList b -> BinList c
zipWith f = go
  where
    -- Recursion
    go xs@(ListNode n l r) ys@(ListNode n' l' r')
         -- If both lists have the same length, recurse assuming it
         -- to avoid comparisons.
       | n == n'   = ListNode n (goEquals l l') (goEquals r r')
         -- If the first list is larger, the second fits entirely in
         -- the left branch of the first.
       | n >  n'   = go l ys
         -- If the second list is larger, the first fits entirely in
         -- the left branch of the second.
       | otherwise = go xs l'
    go xs ys       = ListEnd $ f (head xs) (head ys)
    -- Recursion assuming both lists have the same length
    goEquals (ListNode n l r) (ListNode _ l' r') =
                     ListNode n (goEquals l l') (goEquals r r')
    goEquals xs ys = ListEnd $ f (head xs) (head ys)

-- | /O(n)/. Zip two binary lists in pairs.
zip :: BinList a -> BinList b -> BinList (a,b)
zip = zipWith (,)

-- | /O(n)/. Unzip a binary list of pairs.
unzip :: BinList (a,b) -> (BinList a, BinList b)
unzip (ListEnd (x,y)) = (ListEnd x, ListEnd y)
unzip (ListNode n l r) =
  let (la,lb) = unzip l
      (ra,rb) = unzip r
  in  (ListNode n la ra, ListNode n lb rb)

-----------------------------
-- Transforming from/to lists

-- | /O(log n)/. Calculate the exponent of a positive integer number expressed
--   as a power of two.
exponentInBasisTwo :: Int -> Maybe Int
exponentInBasisTwo 1 = Just 0
exponentInBasisTwo n =
  if even n
     then fmap (+1) $ exponentInBasisTwo $ div n 2
     else Nothing

-- | /O(n)/. Build a binary list from a linked list. If the input list
--   has length different from a power of two, it returns 'Nothing'.
fromList :: [a] -> Maybe (BinList a)
fromList xs = fmap (fromListBuilder xs) $ exponentInBasisTwo $ Prelude.length xs

-- | /O(n)/. This functions builds a binary list from a linked list, assuming
--   the length of the input list is a power of two.
fromListBuilder :: [a] -- ^ Input list
                -> Int -- ^ Length index of the input list
                -> BinList a
fromListBuilder [x] _ = ListEnd x
fromListBuilder xs  n =
  let m = n - 1 -- Length index of a single branch
      (l,r) = splitAt (2^m) xs
  in  ListNode n (fromListBuilder l m) (fromListBuilder r m)

-- | /O(1)/. This is the last exponent that has power of two defined in the type 'Int'.
--
-- /Note: This value is system dependent, since the type 'Int' varies in size/
-- /from system to system./
--
lastExponentOfTwo :: Int
lastExponentOfTwo = 8 * sizeOf (undefined :: Int) - 2

-- | /O(1)/. Calculate the next power of two exponent, if there is any. It is possible
--   to not find a next one since the type 'Int' is finite. If the input is
--   already a power of two, its exponent is returned.
nextExponentOfTwo :: Int -> Maybe Int
nextExponentOfTwo n = find (\i -> n <= 2^i) [0 .. lastExponentOfTwo]

-- | /O(n)/. Build a binary list from a linked list. If the input list
--   has length different from a power of two, fill to the next
--   power of two with a default element.
--
-- /Warning: this function crashes if the input list length is larger than any/
-- /power of two in the type 'Int'./
fromListWithDefault :: a -> [a] -> BinList a
fromListWithDefault e xs =
  let l = Prelude.length xs
  in  case nextExponentOfTwo l of
        Just n -> fromListBuilder (xs ++ Prelude.replicate (2^n - l) e) n
        _ -> error "fromListWithDefault: input list is too big."

-- | /O(n)/. Build a linked list from a binary list.
toList :: BinList a -> [a]
toList = go []
  where
    go xs (ListNode _ l r) = go (go xs r) l
    go xs (ListEnd x) = x : xs

-----------------------
-- Some class instances

instance Show a => Show (BinList a) where
  show = show . toList

instance Functor BinList where
  fmap f (ListNode n l r) = ListNode n (fmap f l) (fmap f r)
  fmap f (ListEnd x) = ListEnd $ f x

