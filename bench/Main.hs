
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL

import Control.DeepSeq
import qualified Data.Foldable as F

import Criterion.Main

instance NFData a => NFData (BinList a) where
  rnf xs = F.foldl1 seq xs `seq` ()

list1024 :: [Int]
list1024 = [1..1024]

list513 :: [Int]
list513 = [1..513]

blist1024 :: BinList Int
blist1024 = BL.generate 10 id

main :: IO ()
main = defaultMain
  [ bgroup "1024"
      [ bench "fromList" $ nf (\i -> const BL.fromList i $ list1024) 0
      , bench "fromListWithDefault" $ nf (\i -> BL.fromListWithDefault i list513) 0
      , bench "generate" $ nf (\i -> BL.generate i id) 10
      , bench "replicate" $ nf (\i -> BL.replicate i (0 :: Int)) 10
      , bench "toListSegment" $ nf (\e -> BL.toListSegment 256 e blist1024) 768
        ]
    ]
