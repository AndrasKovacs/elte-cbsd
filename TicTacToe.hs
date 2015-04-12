
{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}

module TicTacToe (game) where  

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

import Search
import IDS


 
data Cell     = Empty | Filled Player deriving (Eq, Show)
type Move     = (Int, Int)
type GState   = Array Move Cell
type Win      = Maybe Player
newtype Score = Score Int deriving (Eq, Show, Ord, Num)
 
instance Bounded Score where
  maxBound = Score 1
  minBound = Score (-1)
  
size    = 3 -- Don't change this now! 
ixRange = ((1, 1), (size, size))
start   = A.listArray ixRange $ repeat Empty

gameResult :: GState -> Maybe Player
gameResult s = wins^?_head where
  horizontal = chunksOf size $ A.elems s
  vertical   = transpose horizontal
  diagonal   = (map . map) (s!)
               [join zip [1..size], (zip <*> reverse) [1..size]]               
  allLines   = horizontal ++ vertical ++ diagonal  
  wins       = [p | c@(Filled p):cs <- allLines, all (==c) cs]
               
moves :: Player -> GState -> [(GState, Move)]
moves p s = maybe
  [(s // [(ix, Filled p)], ix) | (ix, Empty) <- A.assocs s]
  (const [])
  (gameResult s)

heu :: GState -> Score
heu = maybe 0 (\case PMax -> maxBound; _ -> minBound) . gameResult

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

nextStepTTT ::
  TreeSearch (Timeout Move) Score GState Move ->
  Player -> GState -> IO (Maybe Move)
nextStepTTT alg = ids (alg ((pure.).moves) (pure.heu)) (2*10^6) 10

game ::
  TreeSearch (Timeout Move) Score GState Move
  -> GState -> IO ()
game alg = fix $ \nextRound s -> do  
  putStrLn $ showTable s
  case (all (/=Empty) (A.elems s), gameResult s) of
    (_, Just PMax) -> putStrLn "You won"
    (_, Just PMin) -> putStrLn "You lost"
    (True, _)      -> putStrLn "It's a draw"
    _              -> do
      
      s <- fix $ \tryMove -> do
        inp <- fix $ \tryInp -> maybe
          (putStrLn "Invalid input" >> tryInp)
          pure =<< parseInp <$> getLine
        case s ! inp of
          Empty -> pure $ s // [(inp, Filled PMax)]
          _     -> putStrLn "Cell already filled" >> tryMove
          
      move <- nextStepTTT alg PMin s
      nextRound $ maybe s (\m -> s // [(m, Filled PMin)]) move   

