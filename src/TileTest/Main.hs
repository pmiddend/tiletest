{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Codec.Picture       as Juicy
import           Control.Lens        (each, (%~), (&),
                                      (^.), (^..), (^?!), _1, _2)
import           Control.Lens.At     (ix)
import           Control.Lens.TH     (makeLenses)
import           Data.Array          (Array, array, bounds)
import           Data.List           (nub)
import           Data.Monoid         ((<>))
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Engine       (Picture (..), RenderPositionMode (..),
                                      wrenchPlay)

data TileType = Grass | Dirt deriving(Eq,Show)

data TileProto = TileProto {
      _tileProtoType  :: TileType
    , _tileProtoTrack :: Bool
  }

$(makeLenses ''TileProto)

newtype TileId = TileId (TileType,TileType)

data Tile = Tile {
      _tileNeighbors :: (TileType,TileType,TileType,TileType)
    , _tileId        :: TileId
  }

$(makeLenses ''Tile)

type TileIndex = (Int,Int)

type IndexAndNeighbors = (TileIndex,TileIndex,TileIndex,TileIndex)

type TileNeighborhood = (TileProto,TileProto,TileProto,TileProto)

type PixelArray = Array TileIndex Juicy.PixelRGB8
type TileProtoArray = Array TileIndex TileProto
type TileArray = Array TileIndex Tile

type Track = [TileIndex]

data Terrain = Terrain {
    _tiles :: TileArray
  , _track :: Track
  }

$(makeLenses ''Terrain)

imageToPixelArray :: Juicy.Image Juicy.PixelRGB8 -> PixelArray
imageToPixelArray im = let w = Juicy.imageWidth im
                           h = Juicy.imageHeight im
                       in array ((0,w),(0,h)) [((x,y),Juicy.pixelAt im x y) | x <- [0..w-1], y <- [0..h-1]]

pixelToTileProtoArray :: PixelArray -> TileProtoArray
pixelToTileProtoArray = fmap pixelToTileProto

generateNonBorderIndices :: (TileIndex,TileIndex) -> [IndexAndNeighbors]
generateNonBorderIndices ((wl,wr),(hl,hr)) = [((x,y),(x+1,y),(x,y+1),(x+1,y+1)) | x <- [wl..wr-2],y <- [hl..hr-2]]

tileIdForNeighborhood :: (TileType,TileType,TileType,TileType) -> TileId
tileIdForNeighborhood x = case nub (x ^.. each) of
  [a] -> TileId (a,a)
  [a,b] -> TileId (a,b)
  _ -> error $ "Invalid tile combination: " <> show x

protoToTile :: (TileType,TileType,TileType,TileType) -> Tile
protoToTile ns = Tile {
    _tileNeighbors = ns
  , _tileId = tileIdForNeighborhood ns
  }

tileProtoToTileArray :: TileProtoArray -> TileArray
tileProtoToTileArray a = let newBounds = (bounds a & (_2 . each) %~ (\c -> c - 1))
                             newValues = [(idx ^. _1,protoToTile (idx & each %~ (\i -> a ^?! ix i ^. tileProtoType))) | idx <- generateNonBorderIndices (bounds a)]
                         in array newBounds newValues

tileProtoToTrack :: TileProtoArray -> Track
tileProtoToTrack tp = undefined

pixelToTileProto :: Juicy.PixelRGB8 -> TileProto
pixelToTileProto (Juicy.PixelRGB8 0 120 12) = TileProto Grass False
pixelToTileProto (Juicy.PixelRGB8 0 120 136) = TileProto Grass True
pixelToTileProto (Juicy.PixelRGB8 82 73 32) = TileProto Dirt False
pixelToTileProto (Juicy.PixelRGB8 82 73 107) = TileProto Dirt True
pixelToTileProto (Juicy.PixelRGB8{}) = TileProto Grass False

-- viewportSize = V2 640 480

main :: IO ()
main = do
  imageReadResult <- Juicy.readImage "media/track.png"
--   return ()
  case imageReadResult of
    Left errmsg -> error errmsg
    Right (Juicy.ImageRGB8 omg) -> print $ Juicy.pixelAt omg 7 7
    Right _ -> print "oh no"

  {-do
  wrenchPlay
    "tile test"
    viewportSize
    "media"
    colorsWhite
    0
    1
    (const $ Translate (V2 100 100) $ Rotate (Degrees 90 ^. from degrees) $ Sprite "car" RenderPositionCenter)
    (\_ _ -> 0)
    (\_ _ -> 0)-}
