import System.Random
import System.Exit
import System.Environment

import Data.List
import Data.Maybe
import Data.Foldable (toList)

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

-- TODO Maybe
tileCells :: Tile -> TileWidth
tileCells (Small _) = 1
tileCells (Dbl _) = 2
tileCells (NewRow) = 0 -- Nothing

-- TODO Maybe
tileColor :: Tile -> Color
tileColor (Small c) = c
tileColor (Dbl c) = c
tileColor (NewRow) = -1 -- Nothing

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
  -- putStrLn "== Summary =="
  -- putStrLn "color,size=t"
  -- putStrLn $ concat.intersperse "\n" .map showTileSum $ sumSet floorLayout

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
  putStrLn "  where \n  C -> Number; Tile color variations"
  putStrLn "  S -> Number; Size odds. Tile is either 1 or 2 cell,"
  putStrLn "       and higher number here will give 1cell lower odds"
  putStrLn "  W -> Number; Floor width. Number of cells before new row"
  putStrLn "  H -> Number; Floor height, aka rows"


-- should return uniq set of tiles w counted totals
sumSet :: [Tile] -> [(Tile,Int)]
sumSet tiles =
  toList $ foldl sumTiles countSet tiles
  where
  sumTiles cntSet tile = 
    case lookup tile cntSet of
      Just cnt -> replaceElem tile cnt cntSet
      Nothing -> cntSet
  replaceElem t cnt cs = 
    case idx of
      Just i -> toList $ Seq.update i (t,newCnt) cs'
      Nothing -> cs
    where
      cs' = Seq.fromList cs
      idx = Seq.elemIndexL (t,cnt) cs'
      newCnt = cnt + 1
  countSet = zip (nub tiles) $ repeat 0

showTileSum :: (Tile,Int) -> String
showTileSum (tile,cnt) = 
  show (tileColor tile)  ++ ", " ++ show (tileCells tile)
  ++ " = " ++ show cnt


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


showTile :: Tile -> String
showTile (NewRow) = "\n"
showTile (Small c) = "s" ++ show c
showTile (Dbl c) = "d  " ++ show c

-- TODO derving inside Show class
showTileC :: Tile -> String
showTileC t@(NewRow) = nixEsc 0 ++ showTile t
showTileC t =
  tColor (nicerColor c) ++ showTile t
  where
  c = tileColor t

-- background + HI fg
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
