{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module TileTest.TrackGenerator(
    generateTracks
  , toBooleanList
  , neighborsFromArray
  , showTrack
  ) where

import Data.Array(Array,assocs)
import Control.Lens(Getter,to,makeLenses,(^.),(&),(<>~),_1,_2)
import System.Random(RandomGen)
import TileTest.Random(randomListElement)
import Data.Monoid((<>))
import Data.List((\\),minimum,maximum,find)
import Text.Printf(printf)

type TileIndex = (Int,Int)

-- Ist nicht ganz isomorph zu BooleanList: Die BooleanList hat keine Begrenzungen.
type BooleanArray = Array TileIndex Bool
type BooleanList = [TileIndex]

toBooleanList :: BooleanArray -> BooleanList
toBooleanList = map (^. _1) . filter (^. _2) . assocs

type Track = [TileIndex]

randomStartingPoint :: RandomGen g => g -> BooleanArray -> (TileIndex,g)
randomStartingPoint g = randomListElement g . toBooleanList

-- type TileNeighborFunction = TileIndex -> [TileIndex]
type TileNeighborFunction = Getter TileIndex [TileIndex]

data AlgorithmData = AlgorithmData {
    _trackSoFar :: Track
  }

$(makeLenses ''AlgorithmData)

startingPoint :: Getter AlgorithmData TileIndex
startingPoint = to (head . _trackSoFar)

neighborsFromArray :: BooleanList -> TileNeighborFunction
neighborsFromArray a = to (\(i,j) -> filter (\(r,s) -> i-1 <= r && r <= i+1 && j-1 <= s && s <= j+1 && (i,j) /= (r,s)) a)

generateTracks :: TileNeighborFunction -> TileIndex -> [Track]
generateTracks neighbors sp = generateTracks' neighbors (AlgorithmData [sp]) sp

numberLength :: Int -> Int
numberLength = length . show

showTrack :: Track -> String
showTrack t = let n = (numberLength . length) t
                  minIndices = (minimum . map (^. _1) $ t,minimum . map (^. _2) $ t) :: (Int,Int)
                  maxIndices = (maximum . map (^. _1) $ t,maximum . map (^. _2) $ t) :: (Int,Int)
                  zipped = zip [1..] t :: [(Int,TileIndex)]
                  pfs = "% " <> show n <> "d"
                  r = [[printf pfs (maybe 0 (^. _1) (find ((== (x,y)) . (^. _2)) zipped)) | x <- [(minIndices ^. _1)..(maxIndices ^. _1)]] | y <- [(minIndices ^. _2)..(maxIndices ^. _2)]]
              in unlines . map unwords $ r

-- data TrackTree

generateTracks' :: TileNeighborFunction -> AlgorithmData -> TileIndex -> [Track]
generateTracks' neighbors d previous =
  let ns = previous ^. neighbors
  in if (d ^. startingPoint) `elem` ns && length (d ^. trackSoFar) > 2
     then [d ^. trackSoFar]
     else concatMap (\n -> generateTracks' neighbors (d & trackSoFar <>~ [n]) n) (ns \\ (d ^. trackSoFar))
