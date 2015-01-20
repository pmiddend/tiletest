{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Codec.Picture           as Juicy
import           Control.Lens            (at, both, each, from, over, toListOf,
                                          (%~), (&), (^.), (^..), (^?!), _1, _2)
import           Control.Lens.At         (ix)
import           Control.Lens.TH         (makeLenses)
import           Control.Monad           (mapM_)
import           Data.Array              (Array, array,(!))
import           Data.List               (intercalate, maximumBy, nub)
import Data.Ord(comparing)
import           Data.Monoid             ((<>))
import           Linear.V2
import           Text.Printf             (printf)
import           TileTest.TrackGenerator
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Engine           (Picture (..), RenderPositionMode (..),
                                          SpriteIdentifier, wrenchPlay)
import           Wrench.FloatType
import           Wrench.Rectangle

data TileType = Grass | Dirt deriving(Eq,Show)

data TileProto = TileProto {
      _tileProtoType  :: TileType
    , _tileProtoTrack :: Bool
  }

$(makeLenses ''TileProto)

type TileId = (TileType,TileType)

data Tile = Tile {
      _tileNeighbors :: (TileType,TileType,TileType,TileType)
    , _tileId        :: TileId
  }

$(makeLenses ''Tile)

type TileIndex = (Int,Int)

type Image a = TileIndex -> a

data BoundedImage a = BoundedImage {
    _imageData :: Image a
  , _bounds    :: TileIndex
  }

$(makeLenses ''BoundedImage)

instance Functor BoundedImage where
  fmap f (BoundedImage d b) = BoundedImage (f . d) b

imageValues :: BoundedImage a -> [a]
imageValues (BoundedImage d b) = [d (x,y) | x <- [0..b ^. _1], y <- [0..b ^. _2]]

instance Show a => Show (BoundedImage a) where
  show bi@(BoundedImage d b) =
    let maxl = (maximumBy (comparing (length . show)) . imageValues) bi
        pfs = "% " <> show maxl <> "d"
        ls = [[printf pfs (show (d (x,y))) | x <- [0..(b ^. _1)]] | y <- [0..(b ^. _2)]]
    in unlines . map concat $ ls

type IndexAndNeighbors = (TileIndex,TileIndex,TileIndex,TileIndex)

type TileNeighborhood = (TileProto,TileProto,TileProto,TileProto)

--type PixelArray = Array TileIndex Juicy.PixelRGB8
type PixelArray = BoundedImage Juicy.PixelRGB8
type TileProtoArray = BoundedImage TileProto
--type TileProtoArray = Array TileIndex TileProto
--type TileArray = Array TileIndex Tile
type TileArray = BoundedImage Tile

type Track = [TileIndex]

data Terrain = Terrain {
    _tiles :: TileArray
  , _track :: Track
  }

$(makeLenses ''Terrain)

imageToPixelArray :: Juicy.Image Juicy.PixelRGB8 -> PixelArray
imageToPixelArray im = BoundedImage {
    _imageData = uncurry (Juicy.pixelAt im)
  , _bounds = (Juicy.imageWidth im,Juicy.imageHeight im)
  }

{-
imageToPixelArray :: Juicy.Image Juicy.PixelRGB8 -> PixelArray
imageToPixelArray im = let w = Juicy.imageWidth im
                           h = Juicy.imageHeight im
                       in array ((0,w),(0,h)) [((x,y),Juicy.pixelAt im x y) | x <- [0..w-1], y <- [0..h-1]]
-}

pixelToTileProtoArray :: PixelArray -> TileProtoArray
pixelToTileProtoArray = fmap pixelToTileProto
--pixelToTileProtoArray = fmap pixelToTileProto

generateNeighborhood (x,y) = ((x,y),(x+1,y),(x,y+1),(x+1,y+1))

generateNonBorderIndices :: (TileIndex,TileIndex) -> [IndexAndNeighbors]
generateNonBorderIndices ((wl,wr),(hl,hr)) = [((x,y),(x+1,y),(x,y+1),(x+1,y+1)) | x <- [wl..wr-2],y <- [hl..hr-2]]

tileIdForNeighborhood :: (TileType,TileType,TileType,TileType) -> TileId
tileIdForNeighborhood x = case nub (x ^.. each) of
  [a] -> (a,a)
  [a,b] -> (a,b)
  _ -> error $ "Invalid tile combination: " <> show x

protoToTile :: (TileType,TileType,TileType,TileType) -> Tile
protoToTile ns = Tile {
    _tileNeighbors = ns
  , _tileId = tileIdForNeighborhood ns
  }

-- TODO: Das sieht nach Komonaden aus (focusedimage mit neighbor-Funktion)
tileProtoToTileArray :: TileProtoArray -> TileArray
tileProtoToTileArray a = let newBounds = over each (\c -> c - 1) (a ^. bounds)
                             newValues (x,y) = protoToTile (generateNeighborhood (x,y) & each %~ (\i -> ((a ^. imageData) i) ^. tileProtoType))
                         in BoundedImage newValues newBounds

tileProtoToTrack :: TileProtoArray -> Track
tileProtoToTrack tp = undefined

pixelToTileProto :: Juicy.PixelRGB8 -> TileProto
pixelToTileProto (Juicy.PixelRGB8 0 120 12) = TileProto Grass False
pixelToTileProto (Juicy.PixelRGB8 0 120 136) = TileProto Grass True
pixelToTileProto (Juicy.PixelRGB8 82 73 32) = TileProto Dirt False
pixelToTileProto (Juicy.PixelRGB8 82 73 107) = TileProto Dirt True
pixelToTileProto (Juicy.PixelRGB8{}) = TileProto Grass False

viewportSize = V2 640 480

-- testArray = array ((0,0),(3,3)) [((x,y),True) | x <- [0..3],y <- [0..3]]
-- testArray = array ((0,0),(3,3)) [((x,y),True) | x <- [0..3],y <- [0]]

shortTileType :: TileType -> String
shortTileType Grass = "g"
shortTileType Dirt = "d"

--type TileSearchFunction = Rectangle -> []
spriteIdentifierForTile :: Tile -> SpriteIdentifier
--spriteIdentifierForTile t = map show ((t ^. tileId)^..both)
spriteIdentifierForTile t = concat (map shortTileType (t ^.. tileId.each)) <> "_" <> (concatMap shortTileType (t ^.. tileNeighbors. each))

{-
 - Zuweisung einer absoluten Position zu jedem Tile (tileindex_xy*tilesize) (ist ja immer berechenbar, also als Funktion modellieren)
 - Finde 2D-Indizes der korrekten Tiles raus (viewport_links_oben/tilesize, viewport_rechts_unten/tilesize)
 - Berechne Positionen der ausgewählten Indizes
 - Gucke ins Array um rauszufinden was fürn Tile das ist
 -}
tilesToPicture :: Rectangle -> (TileIndex -> Tile) -> FloatType -> Picture
tilesToPicture viewport ts tileSize =
  let idLeftTop = over each floor ((viewport ^. rectLeftTop) / (V2 tileSize tileSize))
      idRightBottom = over each (floor) ((viewport ^. rectRightBottom) / (V2 tileSize tileSize))
      indices = [(x,y) | x <- [(idLeftTop ^. _x)..(idRightBottom ^. _x)],y <- [(idLeftTop ^. _y)..(idRightBottom ^. _y)]]
      positionForIndex (x,y) = (V2 (fromIntegral x * tileSize) (fromIntegral y * tileSize)) - (viewport ^. rectLeftTop)
  in Pictures $ map (\(x,y) -> Translate (positionForIndex (x,y)) (Sprite (spriteIdentifierForTile (ts (x,y))) RenderPositionTopLeft)) indices

main :: IO ()
main = do
--   print testArray
--   print $ toBooleanList testArray
--  let a = [(0,0),(1,0),(2,0),(2,1),(2,2),(1,2),(0,2),(0,1)]
--  putStrLn (showTree (generateTracks (neighborsFromArray a) (head a)))
--   mapM_ putStrLn $ map showTrack $ generateTracks (neighborsFromArray a) (head a)

  imageReadResult <- Juicy.readImage "media/track.png"
-- --   return ()
  case imageReadResult of
     Left errmsg -> error errmsg
     Right (Juicy.ImageRGB8 omg) -> do
      let pixelArray = imageToPixelArray omg
      print pixelArray
     Right _ -> print "oh no"

  --print (tilesToPicture (rectangleFromPoints (V2 0 0) (V2 192 192)) (\_ -> Tile (Grass,Grass,Dirt,Dirt) (Grass,Dirt)) 96)
  {-
  let nh = (Grass,Grass,Dirt,Dirt)
  putStrLn (spriteIdentifierForTile (Tile nh (tileIdForNeighborhood nh)))



  wrenchPlay
    "tile test"
    viewportSize
    "media"
    colorsWhite
    0
    1
    (const $ Translate (V2 100 100) $ Rotate (Degrees 90 ^. from degrees) $ Sprite "car" RenderPositionCenter)
    (\_ _ -> 0)
    (\_ _ -> 0)
-}
