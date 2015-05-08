{-# LANGUAGE LambdaCase, TupleSections, FlexibleContexts #-}

module CBSD.Potyogos where

import CBSD.Search
import CBSD.Common

import Control.Monad
import Control.Applicative
import Data.List
import Control.Lens
import Data.Ix (inRange, range)
import Data.Array (Array, (!), (//))
import qualified Data.Array as A


type Ix     = (Int, Int)
type Move   = Int
type GState = Array Ix Cell
data Cell   = Empty | Filled Player deriving (Eq, Show)

    
cols, rows :: Int
cols = 7
rows = 6

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
     
--------------------------------------------------    

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

--------------------------------------------------  

dumbHeu :: GState -> Score
dumbHeu s = case result s of
  Win p -> adjustHeu p maxBound
  _     -> 0

smarterHeu :: GState -> Score
smarterHeu s = foldM score 0 neighs ^. chosen where

  groupNeighs :: [[Cell]] -> [([Cell], [Cell])]
  groupNeighs = go [] where
    go ls []        = []
    go ls [g]       = [(g, ls)]
    go ls (g:g':gs) = (g, ls ++ g') : go g (g':gs)      

  groups  = map (group . map (s!)) allIxs
  neighs  = groupNeighs =<< groups

  score :: Score -> ([Cell], [Cell]) -> Either Score Score
  score acc (g@(Filled p:_), ns) =
    case (length g, length $ filter (==Empty) ns) of
      (4, _) -> Left $ adjustHeu p maxBound
      (g, e) -> Right $ acc + (if g + e >= 4 then adjustHeu p (Score g) else 0)
  score acc _ = Right acc

