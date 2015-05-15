{-# LANGUAGE
  LambdaCase, TupleSections, FlexibleContexts,
  OverloadedStrings, ScopedTypeVariables #-}

module CBSD.Potyogos where

import CBSD.Search
import CBSD.Messages.Types

import Control.Applicative
import Control.Lens hiding ((.=), coerce)
import Control.Monad
import Data.Aeson hiding (Array, Result)
import Data.Array (Array, (!), (//))
import Data.Ix (inRange, range)
import Data.List
import Data.List.Split
import Data.Coerce
import qualified Data.Array as A

-- Types
------------------------------------------------------------

type Ix     = (Int, Int)
type Move   = Int
type GState = Array Ix Cell
data Cell   = Empty | Filled Player deriving (Eq, Show)

-- Constants
------------------------------------------------------------
    
cols, rows :: Int
cols = 6
rows = 7

ixRange :: (Ix, Ix)
ixRange = ((0, 0), (rows - 1, cols - 1))

start :: GState
start = A.listArray ixRange $ repeat Empty

publicStart :: GStateJSON
publicStart = coerce start

------------------------------------------------------------

rowIxs, diagIxs, colIxs, allIxs :: [[Ix]]
rowIxs  = [[(i, j) | j <- [0..cols-1]] | i <- [0..rows-1]]
colIxs  = [[(i, j) | i <- [0..rows-1]] | j <- [0..cols-1]]

diagIxs = filter ((>=4).length) $ stripe colIxs ++ stripe (reverse colIxs)
  where
    stripe []           = []
    stripe ([]:xss)     = stripe xss
    stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)
    
    zipCons [] ys         = ys
    zipCons xs []         = map (:[]) xs
    zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys
    
allIxs = rowIxs ++ colIxs ++ diagIxs
       
   
-- Moves     
------------------------------------------------------------

result :: GState -> Result
result s = case wins of
  p:_ -> Win p
  _   -> if ended then Draw else Continue
  where
    runs   = group . map (s!) =<< allIxs
    wins   = [p | run@(Filled p:_) <- runs, length run == 4]
    ended  = all (/=Empty) $ A.elems s

indexOfDrop :: GState -> [Ix] -> Maybe Ix
indexOfDrop s = find ((==Empty) . (s!)) . reverse

indexOfMove :: GState -> Move -> Maybe Ix
indexOfMove s m = indexOfDrop s (range ((0, m), (rows-1, m)))

makeMove :: Player -> GState -> Move -> Maybe GState
makeMove p s m = indexOfMove s m <&> (\ix -> s // [(ix, Filled p)])

moves :: Player -> GState -> [(GState, Move)]
moves p s = case result s of
  Continue -> [(s // [(ix, Filled p)], snd ix) | Just ix <- map (indexOfDrop s) colIxs]
  _        -> []

makeMove' :: Player -> GState -> Move -> (Maybe GState, TurnStatus)
makeMove' p s m = case makeMove p s m of
  Just s -> case result s of
    Win PMax -> (Nothing, PLAYER_1_WON)
    Win PMin -> (Nothing, PLAYER_2_WON)
    Draw     -> (Nothing, DRAW)
    Continue -> (Just s, ONGOING)
  _ -> (Nothing, DRAW)

publicMakeMove ::
  Player -> GStateJSON
  -> MoveJSON -> (Maybe GStateJSON, TurnStatus)
publicMakeMove  p s m = coerce (makeMove' p (coerce s) (coerce m))

publicMoves :: Player -> GStateJSON -> [MoveAndBoard GStateJSON MoveJSON]
publicMoves p s = map (uncurry MoveAndBoard) $ coerce $ moves p (coerce s)


-- Heuristics
------------------------------------------------------------

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

publicDumbHeu :: GStateJSON -> Player -> Int
publicDumbHeu = coerce (\s (_ :: Player) -> dumbHeu s)

publicSmarterHeu :: GStateJSON -> Player -> Int
publicSmarterHeu = coerce (\s (_ :: Player) -> smarterHeu s)


-- Serialization
------------------------------------------------------------

newtype MoveJSON = MoveJSON Move deriving (Eq, Show)

instance ToJSON MoveJSON where
  toJSON (MoveJSON col) = object
    ["from" .= object ["y" .= (0 :: Int), "x" .= (0 :: Int)],
     "to"   .= object ["y" .= (0 :: Int), "x" .= (col :: Int)]]

instance FromJSON MoveJSON where
  parseJSON = withObject "" $ \obj -> do
    to <- obj .: "to"
    MoveJSON <$> (to .: "x")
        
instance ToJSON Cell where
  toJSON Empty         = Number 0
  toJSON (Filled PMax) = Number 1
  toJSON (Filled PMin) = Number 2

instance FromJSON Cell where
  parseJSON = withScientific "" $ \case
    0 -> pure Empty
    1 -> pure $ Filled PMax
    2 -> pure $ Filled PMin
    _ -> empty

newtype GStateJSON = GStateJSON GState deriving (Eq, Show)

instance FromJSON GStateJSON where
  parseJSON val = do
    (table :: [[Cell]]) <- parseJSON val
    pure $ GStateJSON $ A.listArray ixRange $ concat table

instance ToJSON GStateJSON where
  toJSON (GStateJSON arr) = toJSON $ chunksOf cols $ A.elems arr
                  
