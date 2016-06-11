import System.Random
import System.Exit
import System.Environment

import Data.List


data Tile = Small Color 
          | Big Color
            deriving (Show)

type TileWidth = Int
type Color = Int -- TODO data type ??

-- ctor            
newTile :: (TileWidth,Color) -> Tile
newTile (0,c) = Small c
newTile (_,c) = Big c

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
  let tiles = map newTile $ zip widths colors

  -- TODO inject \n after x small-tiles 
  --
  --   (1 big-tile-len == 2 small-tile-len )
  --  Buffer/Flush/State !! !
  putStrLn  $ concat.map showTile $ take 400 tiles
  -- putStr $ concat.map showTile $ zip widths colors
  -- putStr $ intsToLines $ randInts gen 3

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
  putStrLn "Please give us some arguments"
  putStrLn "usage:"
  putStrLn $ "  " ++ prog ++ " TILE-COLORS TILE-SIZES FLOOR-WIDTH FLOOR-HEIGHT"
  -- hmm difficult to exit here

randInts :: StdGen -> Int -> [Int]
randInts g maxInt = 
  let (n,g') = randomR (0,maxInt) g
  in n : randInts g' maxInt

-- TODO derving inside Show class
showTile :: Tile -> String
showTile (Small color) =
  bgColor ( nicerColor color ) ++ "│" ++ show color ++ " "
showTile (Big color) =
  bgColor ( nicerColor color ) ++ "│" ++ show color ++ " . "

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
