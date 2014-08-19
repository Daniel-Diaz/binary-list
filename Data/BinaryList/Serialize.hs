
-- | Serialization methods for binary lists.
module Data.BinaryList.Serialize (
     -- * Simple interface
     encode
   , decode
     -- * Other methods
   , Direction (..)
     -- ** Encoding
   , EncodedBinList (..)
   , encodeBinList
     -- ** Decoding
   , DecodedBinList (..)
   , Decoded (..)
   , fromDecoded
   , decodeBinList
     -- ** ByteString translations
   , encodedToByteString
   , encodedFromByteString
   ) where

-- Binary lists
import Data.BinaryList.Internal
import Data.BinaryList
-- Binary package
import Data.Binary (Binary (..))
import Data.Binary.Put
import Data.Binary.Get
-- Bytestrings
import Data.ByteString.Lazy (ByteString)
-- Utils
import Control.Monad (replicateM)

-- | Encode a binary list using the 'Binary' instance of
--   its elements.
encode :: Binary a => BinList a -> ByteString
encode = encodedToByteString . encodeBinList put FromLeft

-- | Decode a binary list using the 'Binary' instance of
--   its elements. It returns a 'String' in case of
--   decoding failure.
decode :: Binary a => ByteString -> Either String (BinList a)
decode input = encodedFromByteString input >>= fromDecoded . decData . decodeBinList get

-- | Direction of encoding. If the direction is 'FromLeft',
--   the binary list will be encoded from left to right. If
--   the direction is 'FromRight', the binary list will be
--   encoded in the opposite way. Choose a direction according
--   to the part of the list you want to have access earlier.
--   If you foresee reading only a part of the list, either
--   at its beginning or end, an appropiate choice of direction
--   will allow you to avoid decoding the full list.
data Direction = FromLeft | FromRight deriving Eq

-- | A binary list encoded, ready to be written in a file or be
--   sent over a network. It can be directly translated to a
--   'ByteString' using 'encodedToByteString', or restored
--   using 'encodedFromByteString'.
data EncodedBinList =
  EncodedBinList
    { -- | Direction of encoding.
      encDirection :: Direction
      -- | Length index (see 'lengthIndex') of the binary list.
    , encLength :: Int
      -- | Encoded data.
    , encData :: ByteString
      }

-- | Encode a binary list, using a custom serialization for its elements and
--   an user-supplied direction.
encodeBinList :: (a -> Put) -> Direction -> BinList a -> EncodedBinList
encodeBinList f d xs = EncodedBinList d (lengthIndex xs) $
  if d == FromLeft
     then runPut $ foldl (\y x -> y >> f x) (return ()) $ toList xs
     else runPut $ foldr (\x y -> f x >> y) (return ()) $ toList xs

-- | A binary list decoded, from where you can extract a binary list. If the
--   decoding process fails in some point, you still will be able to retrieve
--   the binary list of elements that were decoded successfully before the
--   error.
data DecodedBinList a =
  DecodedBinList
    { -- | Direction of encoding.
      decDirection :: Direction
      -- | Length index (see 'lengthIndex') of the binary list.
    , decLength :: Int
      -- | Decoded data.
    , decData :: Decoded a
      }

-- | The result of decoding a binary list, which produces a list of binary
--   lists of increasing size, ending in either a decoding error or a final
--   binary list. When this is the result of 'decodeBinListIncremental', it
--   contains sublists of order 1, 2, 4, 8, ... up to the order of the total
--   list (unless an error has been encountered first). This sublists are
--   either a section starting at the left, or a section starting at the right,
--   depending on the 'Direction' of encoding.
data Decoded a = -- | Partial binary list, and rest of decoded input.
                 PartialResult (BinList a) (Decoded a)
                 -- | Full binary list and remaining input.
               | FinalResult (BinList a) ByteString
                 -- | A decoding error, with an error message and the remaining input.
               | DecodingError String ByteString

-- | Get the final result of a decoding process, unless it returned an error, in which
--   case this error is returned as a 'String'.
fromDecoded :: Decoded a -> Either String (BinList a)
fromDecoded (PartialResult _ d) = fromDecoded d
fromDecoded (FinalResult xs _) = Right xs
fromDecoded (DecodingError err _) = Left err

-- | Decode an encoded binary list.
--   The result is given as a 'DecodedBinList' value, which can then be
--   queried to get partial results.
decodeBinList :: Get a -> EncodedBinList -> DecodedBinList a
decodeBinList f (EncodedBinList d l b) = DecodedBinList d l $
  case runGetOrFail f b of
    Left (r,_,err) -> DecodingError err r
    Right (r,_,x) -> go r (ListEnd x)
  where
    -- go :: ByteString -- ^ Input data.
    --    -> BinList a -- ^ Accumulated binary list.
    --    -> Decoded a
    go input xs =
       let i = lengthIndex xs
       in  if i == l
              -- If the final length index has been reached, we stop decoding.
              then FinalResult xs input
              -- Otherwise, we read another chunk of data of the same size of
              -- the already decoded data.
              else case runGetOrFail (replicateM (2^i) f) input of
                     Left (r,_,err) -> DecodingError err r
                     Right (r,_,list) ->
                       let -- Binary list of new data
                           ys = fromListBuilder list i
                           -- Since the new data is of the same size of the accumulated
                           -- binary list, we can append safely just using 'ListNode'.
                           -- The appending order is determined by the encoding direction.
                           zs = if d == FromLeft
                                   then ListNode (i+1) xs ys
                                   else ListNode (i+1) ys xs
                           -- The Partial result is returned, and used again recurively to
                           -- build the next binary list.
                       in  PartialResult zs $ go r zs

-- | Translate an encoded binary list to a bytestring.
encodedToByteString :: EncodedBinList -> ByteString
encodedToByteString (EncodedBinList d l b) = runPut $ do
  -- We start with 0 if the direction is left-to-right, and
  -- with 1 if the direction is right-to-left.
  putWord8 $ if d == FromLeft then 0 else 1
  -- Int values are converted to Word64 (note that Word32 does not contain every Int in a
  -- 64-bit system). Then the Word64 value is encoded in big-endian format.
  putWord64be $ fromIntegral l
  putLazyByteString b

-- | Translate a bytestring to an encoded binary list, in case this is possible. Otherwise,
--   it returns a string with a human-readable error.
encodedFromByteString :: ByteString -> Either String EncodedBinList
encodedFromByteString input =
  let p = do w <- getWord8
             d <- case w of
                    0 -> return FromLeft
                    1 -> return FromRight
                    _ -> fail $ "encodedFromByteString: unknown direction " ++ show w
             l <- getWord64be
             return (d,l)
  in  case runGetOrFail p input of
        Left (_,_,err) -> Left err
        Right (r,_,(d,l)) -> Right $ EncodedBinList d (fromIntegral l) r
