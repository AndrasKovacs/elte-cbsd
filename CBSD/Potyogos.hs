{-# LANGUAGE LambdaCase, TupleSections, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module CBSD.Potyogos where

import Control.Monad
import Data.Function
import Data.List
import Data.List.Split
import Control.Lens
import Data.Ix (inRange, range)
import Data.Char
import Data.Maybe
import Text.Printf
import Data.Array (Array, (!), (//))
import qualified Data.Array as A

import CBSD.Search


data Cell     = Empty | Filled Player deriving (Eq, Show)
data Result   = Win Player | Draw | Continue deriving (Eq, Show)
type Ix       = (Int, Int)
type Move     = Int
type GState   = Array Ix Cell
newtype Score = Score Int deriving (Eq, Show, Ord, Num)
 
instance Bounded Score where
  maxBound = Score (maxBound - 1)
  minBound = Score (minBound + 2)
    
cols, rows :: Int
cols    = 7
rows    = 6

ixRange :: (Ix, Ix)
ixRange = ((1, 1), (rows, cols))

start :: GState
start = A.listArray ixRange $ repeat Empty


rowIxs, diagIxs, colIxs, allIxs :: [[Ix]]
rowIxs  = [[(i, j) | j <- [1..cols]] | i <- [1..rows]]
colIxs  = [[(i, j) | i <- [1..rows]] | j <- [1..cols]]

diagIxs = filter ((>=4).length) $ stripe colIxs ++ stripe (reverse colIxs)
  where
    stripe []           = []
    stripe ([]:xss)     = stripe xss
    stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)
    
    zipCons [] ys         = ys
    zipCons xs []         = map (:[]) xs
    zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys
    
allIxs = rowIxs ++ colIxs ++ diagIxs
       
result :: GState -> Result
result s = case wins of
  p:_ -> Win p
  _   -> if ended then Draw else Continue
  where
    runs   = group . map (s!) =<< allIxs
    wins   = [p | run@(Filled p:_) <- runs, length run == 4]
    ended  = all (/=Empty) $ A.elems s
    

dropIndex :: GState -> [Ix] -> Maybe Ix
dropIndex s = find ((==Empty) . (s!)) . reverse

moveIndex :: GState -> Move -> Maybe Ix
moveIndex s m = dropIndex s (range ((1, m), (rows, m)))

makeMove :: Player -> GState -> Move -> Maybe GState
makeMove p s m = moveIndex s m <&> (\ix -> s // [(ix, Filled p)])

moves :: Player -> GState -> [(GState, Move)]
moves p s = case result s of
  Continue -> [(s // [(ix, Filled p)], snd ix) | Just ix <- map (dropIndex s) colIxs]
  _        -> []

dumbHeu :: GState -> Score
dumbHeu s = case result s of
  Win p -> adjustHeu p $ maxBound
  _     -> 0

-- Smarter but buggy currently

-- smarterHeu :: GState -> Score
-- smarterHeu s = sum $ map score neighs where

--   groupNeighs :: [[Cell]] -> [([Cell], [Cell])]
--   groupNeighs = go [] where
--     go ls []        = []
--     go ls [g]       = [(g, ls)]
--     go ls (g:g':gs) = (g, ls ++ g') : go g (g':gs)      

--   groups  = map (group . map (s!)) allIxs
--   neighs  = groupNeighs =<< groups
  
--   score (g@(Filled p:_), ns) =
--     case (length g, length $ filter (==Empty) ns) of
--       (4, _) -> Score $ adjustHeu p maxBound
--       (g, e) -> Score $ if g + e >= 4 then adjustHeu p g else 0
--   score _ = 0


showTable :: GState -> String
showTable s = unlines lines where
  showCell = \case
    Filled PMax -> 'x'
    Filled PMin -> 'o'
    _           -> ' '      
  str   = map showCell $ A.elems s
  lines = take cols ['A'..] : take cols (repeat '-') : chunksOf cols str
     ++ [take cols (repeat '-'), take cols ['A'..]]

parseInp :: String -> Maybe Move
parseInp (col:[]) | inRange ('A', 'G') col = Just (ord col - ord 'A' + 1)
parseInp _ = Nothing


mkSearch = nextMove True ((pure.).moves) (pure.dumbHeu)

-- I don't actually know how hard these are
easy   = mkSearch alphaBeta (1*10^6) 3
medium = mkSearch alphaBeta (1*10^6) 4
hard   = mkSearch alphaBeta (1*10^6) maxBound


game :: (Player -> GState -> IO (Maybe Move)) -> GState -> IO ()
game nextMove = fix $ \nextRound s -> do  
  putStrLn $ showTable s

  case result s of
    Win PMax -> putStrLn "You won"
    Win PMin -> putStrLn "You lost"
    Draw     -> putStrLn "It's a draw"
    Continue -> do
      
      s <- fix $ \tryMove -> do
        m <- fix $ \tryInp -> maybe
          (putStrLn "Invalid input" >> tryInp)
          pure =<< parseInp <$> getLine
        case moveIndex s m of
          Just ix -> pure $ s // [(ix, Filled PMax)]
          _       -> putStrLn "Full column" >> tryMove
                
      maybe
        (nextRound s)
        (nextRound . fromJust . makeMove PMin s)
        =<< nextMove PMin s
