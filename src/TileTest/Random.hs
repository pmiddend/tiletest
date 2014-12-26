module Random(
  randomListElement
  ) where

import System.Random(RandomGen,randomR)

randomListElement :: RandomGen g => g -> [a] -> (a,g)
randomListElement g l = let (i,g') = randomR (0,length l-1) g
                        in (l !! i,g')

