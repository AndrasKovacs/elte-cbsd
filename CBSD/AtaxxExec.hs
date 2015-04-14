
{-# LANGUAGE LambdaCase #-}
module Main where
import CBSD.Ataxx
import Data.Function

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


  








