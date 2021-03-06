 {-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, (!), lookup, fst, snd)
import qualified Prelude as P
import Data.ByteString.Lazy (readFile)
import Data.ByteString (pack)
import Data.Text (unpack)
import Data.Scientific (floatingOrInteger)
import Data.Either (either)
import Data.Map (Map, fromListWith, toList, size)
import qualified Data.Map as M
import Data.HashMap.Strict ((!), lookup)
import qualified Data.Vector as V
import Codec.Archive.Zip
import Data.Aeson
import Data.Maybe
import Codec.Picture
import Codec.Picture.Types
import Data.Word (Word8)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import GHC.Float
import Debug.Trace

-- | A sample zip file containing loaction history taken from my Google account.
--   Note that the file is adpated so that the JSON file containing the location
--   history is the only entry in the zip archive.
locHis :: FilePath
locHis = "LocationHistory.zip"

-- | Return the first element from a four tuple
fst :: (a,b,c,d) -> a
fst (a,_,_,_) = a

-- | Return the second element from a four tuple
snd :: (a,b,c,d) -> b
snd (_,b,_,_) = b

-- | Return the third element from a four tuple
thd :: (a,b,c,d) -> c
thd (_,_,c,_) = c

-- | Return the fourth element from a four tuple
fth :: (a,b,c,d) -> d
fth (_,_,_,d) = d

-- | Create an image using the first image at the first FilePath as
--   the underlying image, using the location history in the second
--   FilePath, where the underlying image represents the area spanning
--   from the first location (South West corner) to the second location
--   (North East corner), offset by the two Int values.
overlay :: FilePath -> FilePath -> (Location,Location,Int,Int) -> FilePath -> IO ()
overlay img lh bounds outFile = do
 esdi <- readImage img
 case esdi of
  Left s -> putStrLn s
  Right di -> do
   let width = dynamicMap imageWidth di
   let height = dynamicMap imageHeight di
   fileByteString <- readFile lh
   let archive = toArchive fileByteString
   let entryByteString = fromEntry (head $ zEntries archive)
   let value = decode entryByteString :: Maybe Value
   case value of
    Nothing -> putStrLn "Error decoding LocationHistory JSON"
    Just (Object hm) -> do
     let ls = getLocations $ hm ! "locations"
     let maxBound = minusL (fst bounds) (snd bounds)
     let ls' = map (minusL (fst bounds)) $ filter (within bounds) ls
     let pix = map (scaleL maxBound width height) ls'
     let pix' = fromListWith (+) (map (\(x,y) -> ((x+thd bounds,y+fth bounds),1)) pix)
     let image = generateImage (getPixelFrom di pix') width height
     print outFile
     saveJpgImage 100 outFile (ImageYCbCr8 image)
    Just v -> putStrLn (info v)


-- | Generate the same overlay the overlay function, but without an
--   underlying image to overlay it on. The first Int argument is used
--   to scale the output image.
justOverlay :: Int -> FilePath -> (Location,Location,Int,Int) -> FilePath -> IO ()
justOverlay scale lh bounds outFile = do
  fileByteString <- readFile lh
  let archive = toArchive fileByteString
  let entryByteString = fromEntry (head $ zEntries archive)
  let value = decode entryByteString :: Maybe Value
  case value of
   Nothing -> putStrLn "Error decoding LocationHistory JSON"
   Just (Object hm) -> do
    let ls = getLocations $ hm ! "locations"
    let ls' = filter (within bounds) ls
    let (minBound,maxBound') = foldr minMax (maxL,minL) ls'
    let maxBound = minusL minBound maxBound'
    let ls'' = map (minusL minBound) ls'
    let width = round $ longitude maxBound * fromIntegral scale
    let height = round $ latitude maxBound * fromIntegral scale
    print (width,height)
    let pix = map (scaleL maxBound width height) ls''
    let pix' = fromListWith (+) (map (\(x,y) -> ((x,y),1)) pix)
    let image = generateImage (getPixel pix') width height
    print outFile
    saveJpgImage 100 outFile (ImageYCbCr8 image)
   Just v -> putStrLn (info v)

-- | Given an underlying image, and a map of visted locations, return
--   either a red pixel denoting a visited location, or the pixel from
--   the underlying image.
getPixelFrom :: DynamicImage -> Map (Int,Int) Int -> Int -> Int -> PixelYCbCr8
getPixelFrom di pix x y = 
 case (M.lookup (x,y) pix) of
  Just h -> convertPixel $ PixelRGB8 maxBound 0 0
  Nothing -> getPixelYCbCr8 di x y

-- | Similar to getPixelFrom but return a black pixel if the location
--   has not been visited.
getPixel :: Map (Int,Int) Int -> Int -> Int -> PixelYCbCr8
getPixel pix x y = 
 case (M.lookup (x,y) pix) of
  Just h -> convertPixel $ PixelRGB8 maxBound 0 0
  Nothing -> convertPixel $ PixelRGB8 0 0 0

-- | Given a minimal location (South West corner) a width and a height,
--   scale the given Location into a position on the width by height grid.
scaleL :: Location -> Int -> Int -> Location -> (Int,Int)
scaleL (Location mLon mLat) w h (Location lon lat) = 
  (round (lat * scaleLat), h - round (lon * scaleLon))
 where
  scaleLat = fromIntegral w / mLat 
  scaleLon = fromIntegral h / mLon

-- | An area covering the UK, with an offset to match a specific 
--   map of the UK (which is no longer in the repo)
uk :: (Location,Location,Int,Int)
uk = (Location 48.892321 (-11.096191),
      Location 59.925110 1.944580,
      (-2),10) 

-- | Another area covering the UK, with an offset to match another 
--   specific map of the UK (which is no longer in the repo)
uk2 :: (Location,Location,Int,Int)
uk2 = (Location 50.392321 (-11.096191),
       Location 59.925110 1.544580,
       (-2),10) 

-- | An area covering the South UK, with an offset to match a specific 
--   map of the South UK (which is no longer in the repo)
southUK :: (Location,Location,Int,Int)
southUK = (Location 49.031686 (-5.894165), 
 	   Location 52.939176 0.453186,
	   (-30),(-5))

-- | An area covering Nova Scotia
ns :: (Location,Location,Int,Int)
ns = (Location 43.068353 (-66.687012),
      Location 47.247419 (-59.106445),
      0,0)

-- | An area covering Halifax, NS
halifax :: (Location,Location,Int,Int)
halifax = (Location 44.613918 (-63.655128),
	   Location 44.682552 (-63.524666),
	   0,0)

-- | An area covering East North America
eastNA :: (Location,Location,Int,Int)
eastNA = (Location 38.957131 (-82.353516),
       	  Location 48.340673 (-58.864746),
	  0,0)

-- | An area covering West Canada
westCanada :: (Location,Location,Int,Int)
westCanada = (Location 48.376200 (-123.458862),
	      Location 49.335802 (-123.020782),
	      0,0)

-- | An area covering the San Francisco Bay Area
bayArea :: (Location,Location,Int,Int)
bayArea = (Location 37.267350 (-122.552490),
	   Location 37.924557 (-121.849365),
	   0,0)

-- | Another area covering the San Francisco Bay Area
bayArea2 :: (Location,Location,Int,Int)
bayArea2 = (Location 37.697350 (-122.512490),
	    Location 37.924557 (-122.230365),
	    0,0)

-- | An area cover the Exumas
exumas :: (Location,Location,Int,Int)
exumas = (Location 23.251104 (-77.173462),
	  Location 24.906035 (-75.020142),
	  0,0)

-- | An area covering London
london :: (Location,Location,Int,Int)
london = (Location 51.436465 (-0.404810),
	  Location 51.775625 0.026631,
	  0,0)

-- | An area covering Nottingham
nottingham :: (Location,Location,Int,Int)
nottingham = (Location 52.908919 (-1.231155),
	      Location 52.961948 (-1.133227),
	      0,0)

-- | An area covering West Europe
westEurope :: (Location,Location,Int,Int)
westEurope = (Location 42.483981 (-4.218750),
	      Location 57.935073 29.794922,
	      0,0)

-- | An area spanning the whole Earth
earth :: (Location,Location,Int,Int)
earth = (Location (-90) (-180),
	 Location 90 180,
	      0,0)

-- | An  area covering St Albans
stAlbans :: (Location,Location,Int,Int)
stAlbans = (Location 51.716465 (-0.354810),
	    Location 51.765625 (-0.273631),
	    0,0)

-- | Another area covering St Albans
stAlbans2 :: (Location,Location,Int,Int)
stAlbans2 = (Location 51.715458 (-0.357971),
	     Location 51.790217 (-0.291367),
	     0,0)

-- | An area covering the South of the UK, specifically the area 
-- covering the triangle formed by Bristol, London and Nottingham
triangle :: (Location,Location,Int,Int)
triangle = (Location 51.30864 (-3.21167),
	    Location 53.119087 (0.732422),
	    0,0)
-- | An area covering Bristol
bristol :: (Location,Location,Int,Int)
bristol = (Location 51.231686 (-2.894165), 
 	   Location 51.539176 (-2.453186),
	   0,0)

-- | Another area covering Bristol
bristol2 :: (Location,Location,Int,Int)
bristol2 = (Location 51.417494 (-2.698059), 
 	    Location 51.487284 (-2.533264),
	    0,0)

-- | An area covering Ottawa
ottawa :: (Location,Location,Int,Int)
ottawa = (Location 43.957131 (-77.8),
       	  Location 48.340673 (-74.5),
	  0,0)

-- | An area covering the M25 motorway
m25 :: (Location,Location,Int,Int)
m25 = (Location 51.2 (-0.6),
       Location 51.8 0.35,
       0,0)

-- | An area covering Innsbruck
innsbruck :: (Location,Location,Int,Int)
innsbruck = (Location 47.204129 11.329193,
             Location 47.318997 11.461716,
             0,0)

-- | An area covering Canary Wharf
cw :: (Location,Location,Int,Int)
cw = (Location 51.482678 (-0.036221),
      Location 51.505423 (-0.00824),
      0,0)

-- | Generate an overlay of the location history covering the UK,
-- overlayed on to the underlying map of the UK (which is no longer in the repo)
main1 :: IO ()
main1 = overlay "UK.jpg" locHis uk "UKoverlay.jpg"

-- | Generate an overlay of the location history covering the South of thet UK,
-- overlayed on to the underlying map of South UK (which is no longer in the repo)
main2 :: IO ()
main2 = overlay "southUK.jpg" locHis southUK "southUKoverlay.jpg"

-- | Generate an image containing the location history within the UK.
main3 :: IO ()
main3 = justOverlay 250 locHis uk "UKjustOverlay.jpg"

-- | Generate various images containing location history for the specified area.
main4 :: IO ()
main4 = do
  justOverlay 5000 locHis m25 "m25.jpg"
  justOverlay 1079 locHis m25 "m25_1024.jpg"
  justOverlay 540 locHis m25 "m25_512.jpg"
  justOverlayColours 1079 locHis m25 "m25_1024colours.jpg"
  --justOverlay 900 locHis ottawa "ottawa.jpg"
  --justOverlay 310 locHis ottawa "ottawa1024.jpg"
  --justOverlayColours 310 locHis ottawa "ottawa1024colours.jpg"
  --justOverlay 250 locHis ns "NS.jpg"
  --justOverlay 150 locHis ns "NS1024.jpg"
  --justOverlayColours 150 locHis ns "NS1024colours.jpg"
  --justOverlay 2500 locHis ns "NSbig.jpg"
  --justOverlay 15000 locHis halifax "halifax.jpg"
  --justOverlayColours 7855 locHis halifax "halifax1024colours.jpg"
  --justOverlay 250 locHis eastNA "eastNA.jpg"
  --justOverlay 52 locHis eastNA "eastNA1024.jpg"
  --justOverlayColours 52 locHis eastNA "eastNA1024colours.jpg"
  --justOverlay 2500 locHis westCanada "westCanada.jpg"
  --justOverlay 5000 locHis bayArea "bayArea.jpg"
  --justOverlay 3666 locHis bayArea2 "bayArea1024.jpg"
  --justOverlayColours 3666 locHis bayArea2 "bayArea1024colours.jpg"
  --justOverlay 2500 locHis exumas "exumas.jpg"
  justOverlay 7500 locHis london "london.jpg"
  justOverlay 2376 locHis london "london1024.jpg"
  justOverlayColours 2376 locHis london "london1024colours.jpg"
  justOverlay 20000 locHis nottingham "nottingham.jpg"
  justOverlay 10500 locHis nottingham "nottingham1024.jpg"
  justOverlayColours 10500 locHis nottingham "nottingham1024colours.jpg"
  justOverlay 250 locHis westEurope "westEurope.jpg"
  justOverlay 1890 locHis uk2 "UK_large.jpg"
  justOverlay 189 locHis uk2 "UK1024.jpg"
  justOverlay 95 locHis uk2 "UK512.jpg"
  justOverlayColours 189 locHis uk2 "UK1024colours.jpg"
  justOverlay 75 locHis earth "earth.jpg"
  justOverlay 8 locHis earth "earth1024.jpg"
  justOverlayColours 8 locHis earth "earth1024colours.jpg"
  justOverlay 129200 locHis stAlbans "stAlbans.jpg"
  justOverlay 12920 locHis stAlbans "stAlbans1024.jpg"
  justOverlay 6460 locHis stAlbans "stAlbans512.jpg"
  justOverlayColours 12920 locHis stAlbans "stAlbans1024colours.jpg"
  justOverlay 7500 locHis bristol "bristol.jpg"
  justOverlay 2328 locHis bristol "bristol1024.jpg"
  justOverlayColours 2328 locHis bristol "bristol1024colours.jpg"
  justOverlay 1000 locHis southUK "southUK_large.jpg"
  justOverlay 171 locHis southUK "southUK1024.jpg"

-- | Generate various images containing location history for the specified area.
main5 :: IO ()
main5 = do
  justOverlay 1000 locHis triangle "triangle.jpg"
  justOverlay 400 locHis triangle "triangle1024.jpg"
  justOverlay 15000 locHis stAlbans2 "stAlbans2.jpg"
  justOverlay 7500 locHis bristol2 "bristol2.jpg"
  justOverlay 10000 locHis innsbruck "innsbruck.jpg"
  justOverlay 30000 locHis cw "canarywharf.jpg"

-- | Subtraction for Locations
minusL :: Location -> Location -> Location
minusL (Location a b) (Location a' b') = Location (a'-a) (b'-b)

-- | Returns whether a given location is within the bounds specified
within :: (Location,Location,Int,Int) -> Location -> Bool
within (min,max,_,_) loc = 
     latitude loc >= latitude min
  && latitude loc <= latitude max
  && longitude loc >= longitude min
  && longitude loc <= longitude max

-- | Returns the minimum and maximum locations given a Location
--   and a previous minimum and maximum locations
minMax :: Location -> (Location,Location) -> (Location,Location)
minMax a (b,c) = (Location minLat minLon,
                  Location maxLat maxLon) where
 minLat = min (latitude a) (latitude b)
 maxLat = max (latitude a) (latitude c)
 minLon = min (longitude a) (longitude b)
 maxLon = max (longitude a) (longitude c)

-- | The minimum of two Maybe values, specialised so that
--   the minimum of Just something and Nothing is the Just value.
minJust :: Ord a => Maybe a -> Maybe a -> Maybe a
minJust Nothing ma = ma
minJust (Just a) mb = case mb of
 Nothing -> Just a
 Just b -> Just (min a b)

-- | The maximum of two Maybe values, specialised so that
--   the maximum of Just something and Nothing is the Just value.
maxJust :: Ord a => Maybe a -> Maybe a -> Maybe a
maxJust Nothing ma = ma
maxJust (Just a) mb = case mb of
 Nothing -> Just a
 Just b -> Just (max a b)

-- | The smallest possible location
minL :: Location
minL = Location (-(1/0)) (-(1/0))

-- | The largest possible location
maxL :: Location
maxL = Location (1/0) (1/0)

-- | Show info about the given JSON value, checking if it contains location history
info :: Value -> String
info val@(Object hm) = show $ getLocation val 
info (Array vec) = info $ V.head vec
info (String text) = "T:" ++ unpack text
info (Number sci) = either (showWith "F:") (showWith "I:") (floatingOrInteger sci)
info (Bool b) = show b
info Null = "Null"

-- | Extract a timestamp from the JSON value
getTimestamp :: Value -> Maybe String
getTimestamp (Object hm) = case (lookup "timestampMs" hm) of
 Just (String text) -> Just $ unpack text
 _ -> Nothing
getTimestamp _ = Nothing

-- | Extract a list of timestamps from the JSON value
getTimestamps :: Value -> [String]
getTimestamps (Array vec) = catMaybes $ V.toList $ V.map getTimestamp vec
getTimestamps _ = []

-- | A helper function to prepend a String when showing a value
showWith :: (Show a) => String -> a -> String
showWith str a = str ++ show a

-- | Extract a location from the JSON value
getLocation :: Value -> Location
getLocation val@(Object hm) = 
 Location 
  (e7 $ getInt $ hm ! "latitudeE7") 
  (e7 $ getInt $ hm ! "longitudeE7") 
  --(fromJust $ getTimestamp val) 
  --(lookup "accuracy" hm >>= return . getInt)
getLocation _ = error "Not a location"

-- | Extract a list of locations from the JSON value
getLocations :: Value -> [Location]
getLocations (Array vec) = V.toList $ V.map getLocation vec
getLocations _ = []

-- | Get an Integral value from the JSON value
getInt :: (Integral a) => Value -> a
getInt (Number sci) = 
 either (error "Not an Int") id (floatingOrInteger sci)
getInt _ = error "Not an Integral"

-- | Scale an Integral by 10^-7
e7 :: (Integral a) => a -> Double
e7 x = (fromIntegral x) / 10000000

-- | A location is a pair of a latitude and a longitude
data Location = Location {
     latitude :: Double,
     longitude :: Double
 } deriving (Eq,Ord)

instance Show Location where
 show (Location lat lon) = show lat ++ "," ++ show lon

-- | Get a specific pixel from a Dynamic Image if it is a ImageYCbCr8
getPixelYCbCr8 :: DynamicImage -> Int -> Int -> PixelYCbCr8
getPixelYCbCr8 (ImageY8    i) x y = error "ImageY8"
getPixelYCbCr8 (ImageY16   i) x y = error "ImageY16"
getPixelYCbCr8 (ImageYF    i) x y = error "ImageYF"
getPixelYCbCr8 (ImageYA8   i) x y = error "ImageYA8"
getPixelYCbCr8 (ImageYA16  i) x y = error "ImageYA16"
getPixelYCbCr8 (ImageRGB8  i) x y = error "ImageRGB8"
getPixelYCbCr8 (ImageRGB16 i) x y = error "ImageRGB16"
getPixelYCbCr8 (ImageRGBF  i) x y = error "ImageRGBF"
getPixelYCbCr8 (ImageRGBA8 i) x y = error "ImageRGBA8"
getPixelYCbCr8 (ImageRGBA16 i) x y = error "ImageRGBA16"
getPixelYCbCr8 (ImageYCbCr8 i) x y = pixelAt i x y
getPixelYCbCr8 (ImageCMYK8 i) x y = error "ImageCMYK8"
getPixelYCbCr8 (ImageCMYK16 i) x y = error "ImageCMYK16"

-- | Similar to justOverlay, but use the number of times each location has been visited
--   to adjust the colour of the pixel at the location. Essentially creating a "heat map"
justOverlayColours :: Int -> FilePath -> (Location,Location,Int,Int) -> FilePath -> IO ()
justOverlayColours scale lh bounds outFile = do
  fileByteString <- readFile lh
  let archive = toArchive fileByteString
  let entryByteString = fromEntry (head $ zEntries archive)
  let value = decode entryByteString :: Maybe Value
  case value of
   Nothing -> putStrLn "Error decoding LocationHistory JSON"
   Just (Object hm) -> do
    let ls = getLocations $ hm ! "locations"
    let ls' = filter (within bounds) ls
    let (minBound,maxBound') = foldr minMax (maxL,minL) ls'
    let maxBound = minusL minBound maxBound'
    let ls'' = map (minusL minBound) ls'
    let width = round $ longitude maxBound * fromIntegral scale
    let height = round $ latitude maxBound * fromIntegral scale
    print (width,height)
    let pix = map (scaleL maxBound width height) ls''
    let pix' = fromListWith (+) (map (\(x,y) -> ((x,y),1)) pix)
    let collection = foldr collect (M.empty) $ map P.snd $ toList pix'
    let scaled = scaleC collection
    print scaled
    let image = generateImage (getPixelColours scaled pix') width height
    print outFile
    saveJpgImage 100 outFile (ImageYCbCr8 image)
   Just v -> putStrLn (info v)

-- | Similar to getPixel, but the colour of the pixel at a visited location is based
--   on the number of times that location has been visited. 
getPixelColours :: Map Int Word8 -> Map (Int,Int) Int -> Int -> Int -> PixelYCbCr8
getPixelColours cols pix x y = 
 case (M.lookup (x,y) pix) of
  Just h -> convertPixel $ PixelRGB8 scaledH 0 (maxBound - scaledH)
   where
     scaledH = cols M.! h
  Nothing -> convertPixel $ PixelRGB8 0 0 0

-- | A helper function for folding over a list of Ints, and collecting the number
--   of times that Int appears in the list.
collect :: Int -> Map Int Int -> Map Int Int
collect i m = case (M.lookup i m) of
 Just j -> M.insert i (j+1) m
 Nothing -> M.insert i 1 m

-- | Scale the values in a Map of Ints so that they fit in Word8s
scaleC :: Map Int Int -> Map Int Word8
scaleC inm = M.fromList outl
 where outl = foldr (\(a,b) ms -> (a,maxBound - round (fromIntegral (2 ^ length ms) * (fromIntegral (maxBound :: Word8) / fromIntegral (2 ^ (size inm - 1))))):ms) [] (M.toList inm)

-- | A main function that opens a zoomable\pan-able view of the location history, starting at St Albans.
main :: IO ()
main = justViewOverlay locHis stAlbans

-- | Given a file containing location history, and a starting area, open a zoomable\pan-able view
justViewOverlay :: FilePath -> (Location,Location,Int,Int) -> IO ()
justViewOverlay lh bounds = do
  fileByteString <- readFile lh
  let archive = toArchive fileByteString
  let entryByteString = fromEntry (head $ zEntries archive)
  let value = decode entryByteString :: Maybe Value
  case value of
   Nothing -> putStrLn "Error decoding LocationHistory JSON"
   Just (Object hm) -> do
    let ls = getLocations $ hm ! "locations"
    let world = (ls,bounds,bounds,ViewPort (0,0) 0 1)
    simulate (InWindow "Location History" (1024,512) (10,10)) black 1 world worldPicture doView
   Just v -> putStrLn (info v)

-- | The world type used for the zoomable\pan-able view
type World = ([Location],(Location,Location,Int,Int),(Location,Location,Int,Int),ViewPort)

-- | Given a World, returns a Picture representing it.
worldPicture :: World -> Picture
worldPicture (ls,_,bounds@(minBound,maxBound,_,_),vp) = Pictures $ map (Color red) ps
 where
  scale = viewPortScale vp
  ls' = filter (within bounds) ls
  maxBound' = minusL minBound maxBound
  ls'' = trace (show $ length ls') $ map (minusL minBound) ls'
  pix = map (scaleL maxBound' 1024 512) ls''
  pix' = fromListWith (+) (map (\(x,y) -> ((x,y),1)) pix)
  ps = map (getPicture scale) $ map P.fst $ M.toList pix'
 
-- | Translate a picture by the given scale and x\y movement
getPicture :: Float -> (Int,Int) -> Picture
getPicture scale (x,y) = Translate (fromIntegral (x-512)) (fromIntegral (512 - (y + 256))) (circle (0.1/scale))

-- | A helper function to update the World depending on user inputs in the ViewPort
doView :: ViewPort -> Float -> World -> World
doView vp _ (ls,bounds@(Location minLat minLon,Location maxLat maxLon,a,b),_,_) = (ls,bounds,(Location minLat' minLon',Location maxLat' maxLon',a,b),vp)
 where 
  scale = 1 / (float2Double $ viewPortScale vp)
  latDist = maxLat - minLat
  latDelta = (latDist - (latDist * scale)) / 2
  lonDist = maxLon - minLon
  lonDelta = (lonDist - (lonDist * scale)) / 2
  minLat' = minLat + latDelta
  minLon' = minLon + lonDelta
  maxLat' = maxLat - latDelta
  maxLon' = maxLon - lonDelta
