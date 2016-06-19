
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
  "<svg xmlns=\"" ++ ns ++ "\"><g>"
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


rectHeight = 50
rectWidth = 50

rect :: Pos -> Int -> Color -> String
rect pos wFactor color = 
  "<rect " ++
  " width=" ++ attr ( rectWidth * wFactor ) ++ 
  " height=" ++ attr rectHeight ++
  " x=" ++ attr (fst pos) ++
  " y=" ++ attr (snd pos) ++
  " style=\"" ++ (style color) ++ "\"" ++
  " >\n"

attr :: Int -> String
attr n = "\"" ++ show n ++ "\""

style :: Int -> String
style c = "stroke:#000"

svgFoot :: String
svgFoot = "<g></svg>"
