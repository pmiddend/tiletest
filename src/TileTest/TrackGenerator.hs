{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module TileTest.TrackGenerator(
    generateTracks
  , toBooleanList
  , neighborsFromArray
  , showTrack
  , finishedTracks
  , showTree
  ) where

import           Control.Lens    (Getter, makeLenses, to, (&), (<>~), (^.), _1,
                                  _2,firstOf)
import           Data.Array      (Array, assocs)
import           Data.List       (find, maximum, minimum, (\\))
import           Data.Monoid     ((<>))
import           System.Random   (RandomGen)
import           Text.Printf     (printf)
import           TileTest.Random (randomListElement)
import Data.Tree(Tree(..))
import Data.Tree.Lens

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

data GenerationStatus = GenerationForked
                      | GenerationFailed
                      | GenerationFinished
                        deriving(Show)

data TrackTreeData = TrackTreeData {
    _algorithmData :: AlgorithmData
  , _generationStatus :: GenerationStatus
  }

$(makeLenses ''TrackTreeData)

type TrackTree = Tree TrackTreeData

{-
data TrackTree = TrackTree {
    _algorithmData    :: AlgorithmData
  , _generationStatus :: GenerationStatus
  , _children         ::  [TrackTree]
  }

instance Show TrackTree where
  show = showTree
-}

showTree :: TrackTree -> String
showTree = showTree' 0
  where showTree' indent t = unlines . map (replicate indent ' ' <>) $
                               [ show (t ^. root . generationStatus) ] ++
                               showTrack (t ^. root . algorithmData . trackSoFar) ++
                               ["Children:"] ++
                               (case t ^. branches of
                                 [] -> []
                                 xs -> map (showTree' (indent+1)) xs)

finishedTracks :: TrackTree -> [Track]
finishedTracks = undefined
--finishedTracks (TrackTree _ GenerationFinished children) =

generateTracks' :: TileNeighborFunction -> AlgorithmData -> TileIndex -> TrackTree
generateTracks' neighbors d previous =
  let ns = previous ^. neighbors
  in if (d ^. startingPoint) `elem` ns && length (d ^. trackSoFar) > 2
     then Node (TrackTreeData d GenerationFinished) []
     else case ns \\ (d ^. trackSoFar) of
            [] -> Node (TrackTreeData d GenerationFailed) []
            [n] -> generateTracks' neighbors (d & trackSoFar <>~ [n]) n
            pns -> Node (TrackTreeData d GenerationForked) (map (\n -> generateTracks' neighbors (d & trackSoFar <>~ [n]) n) pns)

chooseFirstNeighbor :: TileNeighborFunction -> TileIndex -> Maybe TileIndex
chooseFirstNeighbor neighbors sp =
  case sp ^. neighbors of
    [] -> Nothing
    (x:_) -> Just x

generateTracks :: TileNeighborFunction -> TileIndex -> TrackTree
generateTracks neighbors sp = case chooseFirstNeighbor neighbors sp of
                               Nothing -> error "No starting neighbor found"
                               Just sp2 -> generateTracks' neighbors (AlgorithmData [sp,sp2]) sp2
