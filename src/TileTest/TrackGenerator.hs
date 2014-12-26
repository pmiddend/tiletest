{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module TileTest.TrackGenerator(
    generateTracks
  , toBooleanList
  , neighborsFromArray
  , showTrack
  ) where

import           Control.Lens    (Getter, makeLenses, to, (&), (<>~), (^.), _1,
                                  _2)
import           Data.Array      (Array, assocs)
import           Data.List       (find, maximum, minimum, (\\))
import           Data.Monoid     ((<>))
import           System.Random   (RandomGen)
import           Text.Printf     (printf)
import           TileTest.Random (randomListElement)

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

numberLength :: Int -> Int
numberLength = length . show

showTrack :: Track -> [String]
showTrack t = let n = (numberLength . length) t
                  minIndices = (minimum . map (^. _1) $ t,minimum . map (^. _2) $ t) :: (Int,Int)
                  maxIndices = (maximum . map (^. _1) $ t,maximum . map (^. _2) $ t) :: (Int,Int)
                  zipped = zip [1..] t :: [(Int,TileIndex)]
                  pfs = "% " <> show n <> "d"
                  r = [[printf pfs (maybe 0 (^. _1) (find ((== (x,y)) . (^. _2)) zipped)) | x <- [(minIndices ^. _1)..(maxIndices ^. _1)]] | y <- [(minIndices ^. _2)..(maxIndices ^. _2)]]
              in map unwords r

data GenerationStatus = GenerationForked | GenerationFailed | GenerationFinished deriving(Show)

data TrackTree = TrackTree {
    _algorithmData    :: AlgorithmData
  , _generationStatus :: GenerationStatus
  , _children         ::  [TrackTree]
  }

$(makeLenses ''TrackTree)

instance Show TrackTree where
  show = showTree

showTree :: TrackTree -> String
showTree = showTree' 0
  where showTree' indent t = unlines . map (replicate indent ' ' <>) $
                               [ show (t ^. generationStatus) ] ++
                               showTrack (t ^. algorithmData.trackSoFar) ++
                               ["Children:"] ++
                               (case t ^. children of
                                 [] -> []
                                 xs -> map (showTree' (indent+1)) xs)


generateTracks' :: TileNeighborFunction -> AlgorithmData -> TileIndex -> TrackTree
generateTracks' neighbors d previous =
  let ns = previous ^. neighbors
  in if (d ^. startingPoint) `elem` ns && length (d ^. trackSoFar) > 2
     then TrackTree d GenerationFinished []
     else case ns \\ (d ^. trackSoFar) of
            [] -> TrackTree d GenerationFailed []
            [n] -> generateTracks' neighbors (d & trackSoFar <>~ [n]) n
            pns -> TrackTree d GenerationForked (map (\n -> generateTracks' neighbors (d & trackSoFar <>~ [n]) n) pns)

generateTracks :: TileNeighborFunction -> TileIndex -> TrackTree
generateTracks neighbors sp = generateTracks' neighbors (AlgorithmData [sp]) sp
