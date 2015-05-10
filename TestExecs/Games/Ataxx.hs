{-# LANGUAGE
  LambdaCase, FlexibleContexts,
  NoMonomorphismRestriction,
  MonadComprehensions #-}

module Main where

import CBSD.Search
import CBSD.Ataxx

import qualified Data.Vector.Unboxed as UV
import Data.Function
import Data.List.Split
import Data.Char
import Data.Ix
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

  putStrLn "Example move:"
  putStrLn "\"A1 B1\" : move from A1 to B1"
  game difficulty start
  getLine
  pure ()


showCell :: Cell -> Char
showCell = \case
  Filled PMax -> 'X'
  Filled PMin -> 'O'
  Block       -> '#'
  Empty       -> ' '
  
showTable :: GState -> String
showTable s = unlines lines where
  str   = map showCell $ UV.toList s
  lines =
    ("  " ++ take size ['A'..]) :
    zipWith (\i l -> i:'|':l++['|',i]) ['1'..] (chunksOf size str)
    ++ ["  " ++ take size ['A'..]]         

readInp :: Player -> GState -> String -> Maybe Move
readInp p s (from1:from2:' ':to1:to2:[]) =
  let f1 = ord from1 - ord 'A'
      f2 = ord from2 - ord '1'
      t1 = ord to1   - ord 'A'
      t2 = ord to2   - ord '1'
      move = (ix2 f2 f1, ix2 t2 t1)
  in [move |
      all (inRange (0, size - 1)) [f1, f2, t1, t2],
      elem move (map snd $ moves p s)]
readInp _ _ _ = Nothing     

mkSearch = nextMove True ((pure.).moves) (pure.heu)

easy     = mkSearch (orderWith 0 minimax alphaBeta) (1*10^6) 2
medium   = mkSearch (orderWith 0 minimax alphaBeta) (1*10^6) 3
hard     = mkSearch (orderWith 0 minimax alphaBeta) (1*10^6) maxBound

game :: (Player -> GState -> IO (Maybe Move)) -> GState -> IO ()
game nextMove = fix $ \nextRound s -> do  
  putStrLn $ showTable s

  case result PMax s of
    Win PMax -> putStrLn "You won"
    Win PMin -> putStrLn "You lost"
    Draw     -> putStrLn "It's a draw"
    Continue -> do
      
     s <- fix $ \tryMove -> maybe
            (putStrLn "Invalid move" >> tryMove)
            (pure . fromJust . makeMove PMax s)
            =<< readInp PMax s <$> getLine

     putStrLn $ showTable s            
          
     case result PMin s of
       Win PMax -> putStrLn "You won"
       Win PMin -> putStrLn "You lost"
       Draw     -> putStrLn "It's a draw"
       Continue ->
         nextRound . fromJust . makeMove PMin s
         =<< fromJust <$> nextMove PMin s

