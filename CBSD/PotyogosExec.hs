{-# LANGUAGE LambdaCase #-}
module Main where

import CBSD.Potyogos
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

  putStrLn "Try to get four \"x\"-es in a row in vertical, horizontal or diagonal directions."
  putStrLn "Make your move by entering the letter code for a column."
  putStrLn ""
  game difficulty start
  getLine
  pure ()


  








