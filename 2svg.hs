pageW = 1500
pageH = 3000

rectHeight = 50
rectWidth = 50

main = do
  putStrLn svgHead
  readTiles 0
  putStrLn svgFoot

readTiles :: Int -> IO ()
readTiles rowNum = do
  row <- getLine
  if null row 
    then 
      return ()
    else do
      putStrLn $ svgRow 0 rowNum row
      readTiles $ rowNum + 1

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
type Color = (Int)

svgRow :: Int -> Int -> String -> String
svgRow _ _ [] = "\n"
svgRow tileN rowN row = 
  r0 ++ nextCall 
  where
    r0 = rect (rectX,rectY) wFactor color
    nextCall = svgRow (tileN+1) rowN ( drop charsUsed row )
    rectX = tileN * rectWidth
    rectY = rowN * rectHeight
    wFactor = if tileSize == smChar then 1 else 2
    color = if tileSize == smChar then smColor else dblColor
    charsUsed = if tileSize == smChar then 2 else 4
    tileSize = head row
    smChar = 's'
    smColor = read row!!1
    dblColor = read row!!3


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
           0 -> "CCC"
           1 -> "333"
           2 -> "AAA"
           3 -> "C11"
           4 -> "11C"
           5 -> "255"
           6 -> "6A6"
           7 -> "311"
           _ -> "A1A"

svgFoot :: String
svgFoot = 
  "</g></svg>"
