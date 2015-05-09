{-# LANGUAGE
  LambdaCase, FlexibleContexts, NoMonomorphismRestriction #-}

module CBSD.TestGames.Potyogos where

import CBSD.Potyogos
import CBSD.Search

import Data.Array ((//), (!), Array)
import qualified Data.Array as A
import Data.Char
import Data.Function
import Data.Ix
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do  
  putStrLn "Choose difficulty"
  putStrLn "   a) easy"
  putStrLn "   b) medium"
  putStrLn "   c) hard"

  difficulty <- fix $ \again -> do
    getLine >>= \case
      "a" -> pure easy
      "b" -> pure medium
      "c" -> pure hard
      _   -> putStrLn "invalid input" >> again

  putStrLn "Try to get four \"x\"-es in a row in vertical, horizontal or diagonal directions."
  putStrLn "Make your move by entering the letter code for a column."
  putStrLn ""
  game difficulty start
  getLine
  pure ()
  

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
        case indexOfMove s m of
          Just ix -> pure $ s // [(ix, Filled PMax)]
          _       -> putStrLn "Full column" >> tryMove
                
      maybe
        (nextRound s)
        (nextRound . fromJust . makeMove PMin s)
        =<< nextMove PMin s

