{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Codec.Picture       as Juicy
import           Control.Applicative ((<$>))
import           Control.Comonad
import           Control.Lens        (from, (^.))
import           Control.Lens.TH     (makeLenses)
import           Data.List           (sort)
import           Data.Maybe          (fromMaybe, maybeToList)
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG
import           Data.Word           (Word8)
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.Engine       (Picture (..), RenderPositionMode (..),
                                      wrenchPlay)

data BoxedImage a = BoxedImage
  { biWidth  :: !Int
  , biHeight :: !Int
  , biData   :: !(V.Vector a)
  }

instance Functor BoxedImage where
  fmap f (BoxedImage w h d) = BoxedImage w h (fmap f d)

type Pixel = Word8  -- Grayscale
boxImage :: Juicy.Image Juicy.PixelRGB8 -> BoxedImage Pixel
boxImage image = BoxedImage
  { biWidth  = Juicy.imageWidth image
  , biHeight = Juicy.imageHeight image
  , biData   = VG.convert (Juicy.imageData image)
  }

readImage :: FilePath -> IO (BoxedImage Pixel)
readImage filePath = do
  errOrImage <- Juicy.readImage filePath
  case errOrImage of
    Right (Juicy.ImageRGB8 img) -> return (boxImage img)
    Right _                   -> error "readImage: unsupported format"
    Left err                  -> error $ "readImage: could not load image: " ++ err

data FocusedImage a = FocusedImage
  { piBoxedImage :: !(BoxedImage a)
  , piX          :: !Int
  , piY          :: !Int
  }

focus :: BoxedImage a -> FocusedImage a
focus bi
  | biWidth bi > 0 && biHeight bi > 0 = FocusedImage bi 0 0
  | otherwise                         = error "Cannot focus on empty images"

unfocus :: FocusedImage a -> BoxedImage a
unfocus (FocusedImage bi _ _) = bi

instance Functor FocusedImage where
  fmap f (FocusedImage bi x y) = FocusedImage (fmap f bi) x y

instance Comonad FocusedImage where
  extract (FocusedImage bi x y) = biData bi V.! (y * biWidth bi + x)

  extend f (FocusedImage bi@(BoxedImage w h _) x y) = FocusedImage
         (BoxedImage w h $ V.generate (w * h) $ \i ->
             let (y', x') = i `divMod` w
             in f (FocusedImage bi x' y'))
         x y

neighbour :: Int -> Int -> FocusedImage a -> Maybe (FocusedImage a)
neighbour dx dy (FocusedImage bi x y)
     | outOfBounds = Nothing
     | otherwise   = Just (FocusedImage bi x' y')
   where
     x'          = x + dx
     y'          = y + dy
     outOfBounds =
         x' < 0 || x' >= biWidth bi ||
         y' < 0 || y' >= biHeight bi

data TileType = Grass | Dirt

data Tile = Tile {
      _tileType  :: TileType
    , _tileTrack :: Bool
  }

$(makeLenses ''Tile)

pixelToTile :: Juicy.PixelRGB8 -> Tile
pixelToTile (Juicy.PixelRGB8 0 120 12) = Tile Grass False
pixelToTile (Juicy.PixelRGB8 0 120 136) = Tile Grass True
pixelToTile (Juicy.PixelRGB8 82 73 32) = Tile Dirt False
pixelToTile (Juicy.PixelRGB8 82 73 107) = Tile Dirt True
pixelToTile (Juicy.PixelRGB8 _ _ _) = Tile Grass False

--fileToTiles :: DynamicImage -> Array (Int,Int) Tile
--fileToTiles

viewportSize = V2 640 480

main :: IO ()
main = do
  imageReadResult <- readImage "media/track.png"
  return ()
{-
  case imageReadResult of
    Left errmsg -> error errmsg
    Right (Juicy.ImageRGB8 omg) -> print $ Juicy.pixelAt omg 7 7
    Right _ -> print "oh no"-}

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
