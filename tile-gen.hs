import System.Random
import System.Exit
import System.Environment

import Data.List


data Tile = Small Color 
          | Big Color
          | NewRow
            deriving (Show,Eq)

type TileWidth = Int
type Color = Int -- TODO data type ??

-- ctor            
newTile :: (TileWidth,Color) -> Tile
newTile (0,c) = Small c
newTile (_,c) = Big c

-- lovley infinite streams
newTiles :: [TileWidth] -> [Color] -> [Tile]
newTiles ws cs = map (newTile) $ zip ws cs

tileCells :: Tile -> Int
tileCells (Small _) = 1
tileCells (Big _) = 2
tileCells (NewRow) = 0

data InputArgs = InputArgs { 
  tileColors::Int,
  tileSizes::Int,
  floorWidth::Int,
  floorHeight::Int
} deriving (Show)

main = do
  
  args' <- parseArgs
  genA <- getStdGen
  genB <- newStdGen

  -- pull the number for the lotteri !!
  let widths = randInts genB $ tileSizes args' - 1
  let colors = randInts genA $ tileColors args' - 1

  let floorLayout = tileLayout (floorWidth args')
                               (floorHeight args')
                               $ newTiles widths colors

  putStrLn $ concat.map showTile $ floorLayout
  putStrLn $ "=="

-- args or death
parseArgs :: IO InputArgs
parseArgs = do
  let argsLen = 4
  args <- getArgs -- IO
  if length args < argsLen
    then do
      usage
      exitWith $ ExitFailure 1
    else do 
      -- TODO errorhandling
      return InputArgs { 
        tileColors = read $ args !! 0 
      , tileSizes = read $ args !! 1 
      , floorWidth =  read $ args !! 2 
      , floorHeight = read $ args !! 3 }

-- help
usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "Tool that generates floor/tiles (max 2 sizes, 7 colors) from random."
  putStrLn "Please give us these arguments"
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " TILE-COLORS SIZE-ODDS FLOOR-WIDTH FLOOR-HEIGHT"
  -- hmm difficult to exit here

-- summary :: [Tile] -> [(Tile,Int)]
-- summary = foldl (incr) []
--   where
--   incr acc@(t,cnt) tile = case lookup tile acc of 
--                             Just cnt -> swapIn (tile,cnt+1) acc
--                             Nothing -> acc
--   swapIn (


randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt


tileLayout :: Int -> Int -> [Tile] -> [Tile]
tileLayout _ _ [] = []
tileLayout w h tiles 
  | h == 0 = []
  | otherwise = currRow ++ tileLayout w (h-1) futureRows
    where
      currRow = tileRow w tiles
      futureRows = drop currRowLen tiles
      currRowLen = length currRow - 1

-- Inject the NewRow and break stream
tileRow :: Int -> [Tile] -> [Tile]
tileRow _ [] = []
tileRow width (x:xs)  
  | width <= 0 = [NewRow]
  | otherwise = x : tileRow ( width - tileCells x) xs

-- TODO derving inside Show class
showTile :: Tile -> String
showTile (NewRow) = normColor ++ "\n"
showTile (Small color) =
  bgColor ( nicerColor color ) ++ "└ "
showTile (Big color) =
  bgColor ( nicerColor color ) ++ "├   "


-- backto normaal
normColor :: String
normColor = nixEsc 0

-- background
bgColor :: Int -> String
bgColor n =
  let bg = 40
  in 
    nixEsc $ bg + n

-- foreground
fgColor :: Int -> String
fgColor n =
  let fg = 30
  in
    nixEsc $ fg + n

-- avoid red+black as starters
nicerColor :: Int -> Int
nicerColor n 
  | n == 0 = 2 -- green
  | n == 1 = 3 -- yellow
  | n == 2 = 6 -- cyan
  | n == 3 = 7 -- white
  | n == 4 = 0 -- darkGray (black)
  | n == 5 = 4 -- blue
  | n == 6 = 5 -- magents
  | n == 7 = 1 -- red

nixEsc :: Int -> String
-- need `;` and more Int's too work w gb+fg+etc
nixEsc n = "\ESC[" ++ show n ++ "m"


-- intsToLines :: [Int] -> String
-- intsToLines = concat . intersperse "\n" . map show 
