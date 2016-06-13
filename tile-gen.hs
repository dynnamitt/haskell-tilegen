import System.Random
import System.Exit
import System.Environment

import Data.List
import qualified Data.Sequence as Seq


data Tile = Small Color 
          | Dbl Color
          | NewRow
            deriving (Show,Eq)

type TileWidth = Int
type Color = Int -- TODO data type ??

-- ctor            
newTile :: (TileWidth,Color) -> Tile
newTile (0,c) = Small c
newTile (_,c) = Dbl c

-- lovley infinite streams
newTiles :: [TileWidth] -> [Color] -> [Tile]
newTiles ws cs = map (newTile) $ zip ws cs

tileCells :: Tile -> Int
tileCells (Small _) = 1
tileCells (Dbl _) = 2
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
  putStrLn "Tool that generates floor w tiles from random."
  putStrLn "usage:"
  putStrLn $ nixEsc 1 ++ fgColor 3 -- HI fg, yellow
  putStrLn $ "  " ++ prog ++ " <C> <S> <W> <H>" ++ nixEsc 0 -- restore ESC
  putStrLn "  where \n  C -> Tile colors"
  putStrLn "  S -> Size odds ( Tile is either 1 or 2 cell, higher num give 1cell low odds )"
  putStrLn "  W -> Floor width ( Number of cells before new row )"
  putStrLn "  H -> Floor height ( Number of rows )"


  -- hmm difficult to exit here

countedSet :: [Tile] -> [(Tile,Int)]
countedSet tiles = []
  
-- summary = foldl (incr) []
--   where
--   incr acc tile = case lookup tile acc of 
--                     Just cnt -> swapIn (tile, cnt+1) acc
--                     Nothing -> swapIn [(tile,1)] acc
--   swapIn (t,cnt) (x:xs) = 


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
showTile (NewRow) = nixEsc 0 ++ "\n"
showTile (Small color) =
              tColor ( nicerColor color ) ++ "s."
showTile (Dbl color) =
              tColor ( nicerColor color ) ++ "d  ."

-- background + black fg
tColor :: Int -> String
tColor n =
  let bg = 40
      fg = 30
  in 
    nixEsc ( bg + n ) ++ nixEsc 1 ++ nixEsc ( fg + n )

fgColor :: Int -> String
fgColor c =
  let fg = 30
  in
    nixEsc $ fg + c


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

nixEsc :: Int ->String
nixEsc n = "\ESC[" ++ show n ++ "m"


-- intsToLines :: [Int] -> String
-- intsToLines = concat . intersperse "\n" . map show 
