
{-# LANGUAGE
  LambdaCase, GeneralizedNewtypeDeriving,
  FlexibleContexts, TupleSections #-}

module CBSD.TicTacToe where  

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.List.Split
import Data.Char
import Data.Ix
import Data.Function

import Data.Array (Array, (//), (!))
import qualified Data.Array as A

import CBSD.Search
 
data Cell     = Empty | Filled Player deriving (Eq, Show)
data Result   = Win Player | Draw | Continue deriving (Eq, Show)
type Move     = (Int, Int)
type GState   = Array Move Cell
newtype Score = Score Int deriving (Eq, Show, Ord, Num)
 
instance Bounded Score where
  maxBound = Score (maxBound - 1)
  minBound = Score (minBound + 2)
  
size    = 3 -- Don't change this! We're not actually parametric in size now.
ixRange = ((1, 1), (size, size))
start   = A.listArray ixRange $ repeat Empty

result :: GState -> Result
result s = case wins of
  p:_  -> Win p
  _    -> if ended then Draw else Continue
  where
    horizontal = chunksOf size $ A.elems s
    vertical   = transpose horizontal
    diagonal   = (map . map) (s!)
                 [join zip [1..size], (zip <*> reverse) [1..size]]               
    allLines   = horizontal ++ vertical ++ diagonal  
    wins       = [p | c@(Filled p):cs <- allLines, all (==c) cs]
    ended      = all (/=Empty) (A.elems s)
               
moves :: Player -> GState -> [(GState, Move)]
moves p s = case result s of
  Continue -> [(s // [(ix, Filled p)], ix) | (ix, Empty) <- A.assocs s]
  _        -> []

heu :: GState -> Score
heu s = case result s of
  Win p -> adjustHeu p $ maxBound
  _     -> 0

showTable :: GState -> String
showTable s = unlines lines where
  showCell = \case
    Filled PMax -> 'x'
    Filled PMin -> 'o'
    _           -> ' '      
  str   = map showCell $ A.elems s
  lines = (take (size + 1) (' ':['A'..])) :
          zipWith (:) ['1'..] (chunksOf size str)

parseInp :: String -> Maybe Move
parseInp (col:row:[])
  | inRange ('A', 'C') col && inRange ('1', '3') row  = 
      Just (ord row - ord '1' + 1 , ord col - ord 'A' + 1)
parseInp _ = Nothing

nextMoveTTT' = nextMove True ((pure.).moves) (pure.heu)

nextMoveTTT :: Player -> GState -> IO (Maybe Move)
nextMoveTTT  = nextMoveTTT' alphaBeta (1*10^6) 10

game :: GState -> IO ()
game = fix $ \nextRound s -> do  
  putStrLn $ showTable s
  
  case result s of
    Win PMax -> putStrLn "You won"
    Win PMin -> putStrLn "You lost"
    Draw     -> putStrLn "It's a draw"
    Continue -> do
      
      s <- fix $ \tryMove -> do
        inp <- fix $ \tryInp -> maybe
          (putStrLn "Invalid input" >> tryInp)
          pure =<< parseInp <$> getLine
        case s ! inp of
          Empty -> pure $ s // [(inp, Filled PMax)]
          _     -> putStrLn "Cell already filled" >> tryMove
          
      maybe
        (nextRound s)
        (\move -> nextRound $ s // [(move, Filled PMin)])
        =<< nextMoveTTT PMin s

