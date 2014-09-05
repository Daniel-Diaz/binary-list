
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL

import Control.DeepSeq
import qualified Data.Foldable as F

import Criterion.Main

instance NFData a => NFData (BinList a) where
  rnf xs = F.foldl1 seq xs `seq` ()

main :: IO ()
main = defaultMain
  [ bgroup "1024"
      [ bench "fromList" $ nf (\i -> BL.fromList [1..i]) (1024 :: Int)
      , bench "fromListWithDefault" $ nf (\i -> BL.fromListWithDefault 0 [1..i]) (513 :: Int)
      , bench "generate" $ nf (\i -> BL.generate i id) 10
      , bench "replicate" $ nf (\i -> BL.replicate i (0 :: Int)) 10
        ]
    ]