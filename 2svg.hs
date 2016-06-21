import Data.Char

pageW = 1100
pageH = 1100

rectHeight = 20
rectWidth = 20

main = do
  putStrLn svgHead
  parseTiles 0
  putStrLn svgFoot

parseTiles :: Int -> IO ()
parseTiles rowNum = do
  row <- getLine
  let rowBW = removeEsc row
  -- let row' = filter isCodeChar row
  if null rowBW || length rowBW < 2
    then 
      return ()
    else do
      putStrLn $ svgRow 0 rowNum rowBW
      parseTiles $ rowNum + 1

svgHead :: String
svgHead = 
  "<svg xmlns=\"" ++ ns ++ "\"" ++
  " version=\"1.1\" " ++
  " width=\"" ++ show pageW ++ "\"" ++
  " height=\"" ++ show pageH ++ "\"" ++
  " ><defs>" ++ fillGrps ++ "</defs><g>"
  where
    ns = "http://www.w3.org/2000/svg"
    
svgFoot :: String
svgFoot = 
  "</g></svg>"

-- works w ESC[ codes from 00-99 ending w 'm'
removeEsc :: String -> String
removeEsc (a:b:c:d:xs)  
  | a == '\ESC' && b == '[' = 
    if d == 'm'
    then removeEsc xs -- one figure code
    else removeEsc $ drop 1 xs -- two figure code
  | otherwise = a : removeEsc ( b:c:d:xs ) -- aproved char a
removeEsc str = str -- aprove all

type Pos = (Int,Int) -- x,y
type Color = Int

data Tile = Tile { cellSpan::Int, color::Color }
            deriving (Show)

tile :: String -> Tile
tile [] = error "String illegal, empty!"
tile (x:[]) = error $ "Need 2 chars! Got '" ++ [x] ++ "'"
tile (x:xs) = Tile span' color'
  where
  span' = if x == 's' then 1 else 2
  color' = if isDigit colorChar 
            then read [colorChar] :: Color
            else error $ "Illegal color num '" ++ [colorChar] ++ "'"
  colorChar = if x == 's' then head xs else xs!!2

tileCharLen :: Tile -> Int
tileCharLen (Tile 1 _ ) = 2
tileCharLen (Tile 2 _ ) = 4
tileCharLen _ = error "møkka tile"


svgRow :: Int -> Int -> String -> String
svgRow _ _ [] = "\n"
svgRow _ _ (x:[]) = error "crap in;" ++ [x]
svgRow xOffset rowN row = 
  r0 ++ nextCall 
  where
    r0 = rect (rectX,rectY) (cellSpan t) (color t)
    nextCall = svgRow nextOffset rowN ( drop charsUsed row )
    nextOffset = cellSpan t + xOffset
    rectX = xOffset * rectWidth
    rectY = rowN * rectHeight
    t = tile row
    charsUsed = tileCharLen t


rect :: Pos -> Int -> Color -> String
rect pos wFactor color = 
  "<rect " ++
  " class=\"swatch_" ++ show color ++ " spans_" ++ show wFactor ++ "\"" ++
  " width=" ++ attr ( rectWidth * wFactor ) ++ 
  " height=" ++ attr rectHeight ++
  " x=" ++ attr (fst pos) ++
  " y=" ++ attr (snd pos) ++
  " style=\"" ++ (style color) ++ "\"" ++
  " />\n"

attr :: Int -> String
attr n = 
  "\"" ++ show n ++ "\""

style :: Color -> String
style c =
  "fill:url(#" ++ colorId c ++ ");" ++
  "stroke:#999;"

fillGrps :: String
fillGrps =
  concat $ map linearGradient [0..9]

linearGradient :: Color -> String
linearGradient c =
  "<linearGradient" ++
  " id=\"" ++ colorId c ++ "\">\n" ++
  stop c ++
  "</linearGradient>"

stop :: Color -> String
stop c =
  "<stop" ++
  " offset=\"0\"" ++
  " style=\"stop-opacity:1;" ++
  "stop-color:#" ++ hexd c ++ "\" />"

colorId :: Color -> String
colorId c = "_c" ++ show c

hexd :: Color -> String
hexd c = case c of
           0 -> "111"
           1 -> "333"
           2 -> "666"
           3 -> "999"
           4 -> "AAA"
           5 -> "CCC"
           6 -> "EEE"
           7 -> "F4F4F4"
           _ -> "A11"


