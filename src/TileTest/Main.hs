{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Codec.Picture           as Juicy
import           Control.Lens            (at, both, each, from, over, toListOf,
                                          (%~), (&), (^.), (^..), (^?!), _1, _2,_3,_4,(+~),(-~),(.~))
import           Control.Lens.At         (ix)
import           Control.Lens.TH         (makeLenses)
import           Control.Monad           (mapM_)
import Data.Traversable(traverse)
import           Data.Array              (Array, array,(!))
import Data.Maybe(mapMaybe)
import           Data.List               (intercalate, maximumBy, nub,sort)
import Data.Ord(comparing)
import Debug.Trace(traceShowId)
import           Data.Monoid             ((<>))
import           Linear.V2
import           Linear.Vector
import           Text.Printf             (printf)
import           TileTest.TrackGenerator
import           Wrench.Angular
import           Wrench.Time
import           Wrench.Color
import           Wrench.Engine           (Picture (..), RenderPositionMode (..),
                                          SpriteIdentifier, wrenchPlay,Event(..),KeyMovement(..),Keysym(..))
import           Wrench.FloatType
import           Wrench.Point
import           Wrench.Rectangle
import qualified Wrench.Keycode as KC
import Control.Applicative
import Data.Bool.Extras(bool)

data TileType = Grass | Dirt deriving(Eq,Show,Ord)

data TileProto = TileProto {
      _tileProtoType  :: TileType
    , _tileProtoTrack :: Bool
  }

$(makeLenses ''TileProto)

instance Show TileProto where
  show tp = [bool ' ' 'T' (tp ^. tileProtoTrack),bool 'G' 'D' (tp ^. tileProtoType == Dirt)]

type TileId = (TileType,TileType)

data Tile = Tile {
      _tileNeighbors :: (TileType,TileType,TileType,TileType)
    , _tileId        :: TileId
  }

$(makeLenses ''Tile)

instance Show Tile where
  show t = [showTile (t ^. (tileNeighbors . _1)),showTile (t ^. (tileNeighbors . _2)),showTile (t ^. (tileNeighbors . _3)),showTile (t ^. (tileNeighbors . _4))]
--    show t = (t ^. tileNeighbors) & each %~ showTile
    where showTile Grass = 'G'
          showTile Dirt = 'D'

type TileIndex = (Int,Int)

type Image a = TileIndex -> a

data BoundedImage a = BoundedImage {
    _imageData :: Image a
  , _bounds    :: TileIndex
  }

$(makeLenses ''BoundedImage)

instance Functor BoundedImage where
  fmap f (BoundedImage d b) = BoundedImage (f . d) b

type MaybeImage a = TileIndex -> Maybe a

boundedImageToMaybeGetter :: BoundedImage a -> MaybeImage a
boundedImageToMaybeGetter bi (x,y) =
  if x < 0 || y < 0 || x > (bi ^. bounds ^. _1) || y > (bi ^. bounds ^. _2)
    then Nothing
    else Just $ (bi ^. imageData) (x,y)

imageValues :: BoundedImage a -> [a]
imageValues (BoundedImage d b) = [d (x,y) | x <- [0..b ^. _1], y <- [0..b ^. _2]]

instance Show a => Show (BoundedImage a) where
  show bi@(BoundedImage d b) =
    let maxl = (maximum . map (length . show) . imageValues) bi
        pfs = "% " <> show (maxl + 1) <> "s"
        ls = [[printf pfs (show (d (x,y))) | x <- [0..(b ^. _1)]] | y <- [0..(b ^. _2)]]
--     in show maxl
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
  , _bounds = (Juicy.imageWidth im-1,Juicy.imageHeight im-1)
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
                             newValues (x,y) = protoToTile (generateNeighborhood (x,y) & each %~ (\i -> (a ^. imageData) i ^. tileProtoType))
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
spriteIdentifierForTile t = concatMap shortTileType (sort (t ^.. tileId.each)) <> "_" <> concatMap shortTileType (t ^.. tileNeighbors. each)

{-
 - Zuweisung einer absoluten Position zu jedem Tile (tileindex_xy*tilesize) (ist ja immer berechenbar, also als Funktion modellieren)
 - Finde 2D-Indizes der korrekten Tiles raus (viewport_links_oben/tilesize, viewport_rechts_unten/tilesize)
 - Berechne Positionen der ausgewählten Indizes
 - Gucke ins Array um rauszufinden was fürn Tile das ist
 -}
tilesToPicture :: Rectangle -> (TileIndex -> Maybe Tile) -> FloatType -> Picture
tilesToPicture viewport ts tileSize =
  let idLeftTop = over each floor ((viewport ^. rectLeftTop) / V2 tileSize tileSize)
      idRightBottom = over each floor ((viewport ^. rectRightBottom) / V2 tileSize tileSize)
      indices = [(x,y) | x <- [(idLeftTop ^. _x)..(idRightBottom ^. _x)],y <- [(idLeftTop ^. _y)..(idRightBottom ^. _y)]]
      positionForIndex (x,y) = V2 (fromIntegral x * tileSize) (fromIntegral y * tileSize) - (viewport ^. rectLeftTop)
  in Pictures $ mapMaybe (\(x,y) -> ts (x,y) >>= \tile -> return $ Translate (positionForIndex (x,y)) (Sprite (spriteIdentifierForTile tile) RenderPositionTopLeft)) indices

getRed :: Juicy.PixelRGB8 -> Juicy.Pixel8
getRed (Juicy.PixelRGB8 r _ _) = r

type ViewportMovement = (Int,Int)

data EngineState = EngineState {
    _esTiles :: MaybeImage Tile
  , _esViewport :: Rectangle
  , _esViewportMovement :: Point
  }

$(makeLenses ''EngineState)

engineStateToPicture :: EngineState -> Picture
engineStateToPicture es = tilesToPicture (es ^. esViewport) (es ^. esTiles) 96

engineStateEventHandler :: Event -> EngineState -> EngineState
engineStateEventHandler event state = case event of
  Keyboard KeyUp _ (Keysym KC.Up _) -> state & (esViewportMovement . _y) .~ 0
  Keyboard KeyDown _ (Keysym KC.Up _) -> state & (esViewportMovement . _y) .~ (-1)
  Keyboard KeyUp _ (Keysym KC.Down _) -> state & (esViewportMovement . _y) .~ 0
  Keyboard KeyDown _ (Keysym KC.Down _) -> state & (esViewportMovement . _y) .~ 1
  Keyboard KeyUp _ (Keysym KC.Left _) -> state & (esViewportMovement . _x) .~ 0
  Keyboard KeyDown _ (Keysym KC.Left _) -> state & (esViewportMovement . _x) .~ (-1)
  Keyboard KeyUp _ (Keysym KC.Right _) -> state & (esViewportMovement . _x) .~ 0
  Keyboard KeyDown _ (Keysym KC.Right _) -> state & (esViewportMovement . _x) .~ 1
  _ -> state

engineStateTickHandler :: TimeDelta -> EngineState -> EngineState
engineStateTickHandler td state = state & (esViewport . rectLeftTop) +~ traceShowId (100 * toSeconds td *^ (state ^. esViewportMovement))

main :: IO ()
main = do
  imageReadResult <- Juicy.readImage "media/bigmap.png"
  case imageReadResult of
     Left errmsg -> error errmsg
     Right (Juicy.ImageRGB8 omg) -> do
      let pixelArray = imageToPixelArray omg
      let tileProtoArray = pixelToTileProtoArray pixelArray
      let tileArray = tileProtoToTileArray tileProtoArray
      let viewport = V2 1024 768
--       print tileArray
      let maybeImage = boundedImageToMaybeGetter tileArray
--       let picture = tilesToPicture (rectangleFromPoints (V2 0 0) viewport) maybeImage 96
--       case picture of
--         Pictures xs -> print (length xs)
--         _ -> print picture
      wrenchPlay
        "window title"
        viewport
        "media"
        colorsWhite
        (EngineState maybeImage (rectangleFromPoints (V2 0 0) viewport) (V2 0 0))
        30
        engineStateToPicture
        engineStateEventHandler
        engineStateTickHandler
     Right _ -> error "invalid image format"

