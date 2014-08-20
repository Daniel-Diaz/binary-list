
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
  , replicateA
  , replicateAR
    -- * Queries
  , lengthIndex
  , length
  , lookup
  , head
  , last
  , minimum
  , maximum
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

import Prelude hiding ( length,lookup,replicate,head,last,zip,unzip,zipWith,reverse
                      , minimum, maximum
                        )
import qualified Prelude
import Foreign.Storable (sizeOf)
import Data.List (find)
import Data.BinaryList.Internal
import Control.Applicative (Applicative (..),(<$>))
import Control.Applicative.Backwards


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
replicate n x = go n
  where
    go 0 = ListEnd x
    go i = let b = go (n-1) -- Both branches of the binary list
           in  ListNode i b b -- Note that both branches are the same shared object

{-# RULES
      "Data.BinaryList: fmap/replicate"
         forall f n x. fmap f (replicate n x) = replicate n (f x)
  #-}

-- | Calling @replicateA n f@ builds a binary list collecting the results of
--   executing the applicative action @f@ @2^n@ times.
replicateA :: Applicative f => Int -> f a -> f (BinList a)
replicateA n f = go n
  where
    go 0 = ListEnd <$> f
    go i = let b = go (i-1)
           in  ListNode <$> pure i <*> b <*> b

-- | The same as 'replicateA', but the actions are executed in reversed order.
replicateAR :: Applicative f => Int -> f a -> f (BinList a)
replicateAR n = forwards . replicateA n . Backwards

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

{-# RULES
      "Data.BinaryList: reverse/reverse"
         forall xs. reverse (reverse xs) = xs
  #-}

-- | /O(n)/. Retrieves the minimum element of a binary list.
minimum :: Ord a => BinList a -> a
minimum (ListEnd x) = x
minimum (ListNode _ l r) = min (minimum l) (minimum r)

-- | /O(n)/. Retrieves the maximum element of a binary list.
maximum :: Ord a => BinList a -> a
maximum (ListEnd x) = x
maximum (ListNode _ l r) = max (maximum l) (maximum r)

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
-- /power of two in the type 'Int'. However, this is very unlikely./
fromListWithDefault :: a -> [a] -> BinList a
fromListWithDefault e xs =
  let l = Prelude.length xs
  in  case nextExponentOfTwo l of
        Just n -> fromListBuilderWithDefault e xs l n
        _ -> error "fromListWithDefault: input list is too big."

-- | /O(n)/. Build a binary list from any linked list, providing a default element
--   to use when in need of completing elements to the next power of two,
--   and the length index of the output binary list.
fromListBuilderWithDefault :: a -- ^ Default element
                           -> [a] -- ^ Input list
                           -> Int -- ^ Lenght of the input list
                           -> Int -- ^ Length index of the list expanded
                                  --   to the next power of two
                           -> BinList a
fromListBuilderWithDefault e = go
  where
    go [] _ n = replicate n e
    go xs _ 0 = ListEnd $ Prelude.head xs -- Hopefully we can avoid this case?
    go xs l n =
      let m = n - 1
          l' = 2^m
          (ys,zs) = splitAt l' xs
      in  if l <= l'
             then ListNode n (go xs l m) (replicate m e)
             else ListNode n (fromListBuilder ys m) (go zs (l - l') m)

-- | /O(n)/. Build a linked list from a binary list.
toList :: BinList a -> [a]
toList = go []
  where
    go xs (ListNode _ l r) = go (go xs r) l
    go xs (ListEnd x) = x : xs

-----------------------------
-- Show and Functor instances

instance Show a => Show (BinList a) where
  show = show . toList

instance Functor BinList where
  fmap f (ListNode n l r) = ListNode n (fmap f l) (fmap f r)
  fmap f (ListEnd x) = ListEnd $ f x
