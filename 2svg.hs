import Data.Char

pageW = 1500
pageH = 3000

rectHeight = 50
rectWidth = 50

main = do
  putStrLn svgHead
  parseTiles 0
  putStrLn svgFoot

parseTiles :: Int -> IO ()
parseTiles rowNum = do
  row <- getLine
  if null row || length row < 2
    then 
      return ()
    else do
      putStrLn $ svgRow 0 rowNum row
      parseTiles $ rowNum + 1

svgHead :: String
svgHead = 
  "<svg xmlns=\"" ++ ns ++ "\"" ++
  " version=\"1.1\" " ++
  " width=\"" ++ show pageW ++ "\"" ++
  " height=\"" ++ show pageH ++ "\"" ++
  " ><g>"
  where
    ns = "http://www.w3.org/2000/svg"

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
  "fill:#" ++ (hexd c) ++ ";" ++
  "stroke:#000;"

hexd :: Color -> String
hexd c = case c of
           0 -> "101010"
           1 -> "220022"
           2 -> "333"
           3 -> "C11"
           4 -> "11C"
           5 -> "255"
           6 -> "6A6"
           7 -> "311"
           _ -> "A1A"

svgFoot :: String
svgFoot = 
  "</g></svg>"
