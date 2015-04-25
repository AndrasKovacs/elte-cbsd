{-# LANGUAGE LambdaCase, TupleSections, FlexibleContexts, OverloadedStrings #-}

module CBSD.Potyogos where

import Control.Monad
import Control.Applicative
import Data.Function
import Data.List
import Data.List.Split
import Control.Lens
import Data.Ix (inRange, range)
import Data.Char
import Data.Maybe
import Data.Array (Array, (!), (//))
import qualified Data.Array as A

import Data.Aeson hiding (Result, Array)
import Data.Aeson.Types hiding (Result, Array)

import CBSD.Search
import CBSD.Common

type Ix     = (Int, Int)
type Move   = Int
type GState = Array Ix Cell
data Cell   = Empty | Filled Player deriving (Eq, Show)

    
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

showTable :: GState -> String
showTable s = unlines lines where
  showCell = \case
    Filled PMax -> 'x'
    Filled PMin -> 'o'
    _           -> ' '      
  str   = map showCell $ A.elems s
  lines = take cols ['A'..] : take cols (repeat '-') : chunksOf cols str
     ++ [take cols (repeat '-'), take cols ['A'..]]

     
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


--------------------------------------------------

instance ToJSON Cell where
  toJSON (Filled p) = toJSON p
  toJSON Empty      = Number 0

instance FromJSON Cell where
  parseJSON v =
        (Filled <$> parseJSON v)
    <|> (Empty  <$ withScientific "" (guard . (==0)) v)

--------------------------------------------------


parseInp :: String -> Maybe Move
parseInp (col:[]) | inRange ('A', 'G') col = Just (ord col - ord 'A' + 1)
parseInp _ = Nothing

mkSearch = nextMove True ((pure.).moves) (pure.smarterHeu)
easy   = mkSearch alphaBeta (1*10^6) 2
medium = mkSearch alphaBeta (1*10^6) 4
hard   = mkSearch (orderWith 0 minimax alphaBeta) (1*10^6) maxBound

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

