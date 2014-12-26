module TileTest.TrackGenerator(
  ) where

import Data.Array(Array,indices)
import System.Random(RandomGen)
import TileTest.Random(randomListElement)

type TileIndex = (Int,Int)

-- Ist nicht ganz isomorph zu BooleanList: Die BooleanList hat keine Begrenzungen.
type BooleanArray = Array TileIndex Bool
type BooleanList = [TileIndex]

toBooleanList :: BooleanArray -> BooleanList
toBooleanList = indices

type Track = [TileIndex]

randomStartingPoint :: RandomGen g => BooleanArray -> (TileIndex,g)
randomStartingPoint = undefined

generateTracks :: BooleanArray -> [Track]
generateTracks = undefined
