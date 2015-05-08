
{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns, RankNTypes, FlexibleContexts #-}

module CBSD.Search (
  
    Player(..)
  , Search
  , Timeout(..)
  , Depth
  , adjustHeu
  , switch
    
  , nextMove    
  , minimax
  , alphaBeta
  , orderWith
  ) where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Ord
import Control.Monad.Except
import Control.Monad.IO.Class
import System.Timeout.Returning
import Text.Printf
import Data.Aeson

type Depth = Int
data Player = PMax | PMin deriving (Eq, Show)

instance ToJSON Player where
  toJSON = \case PMax -> Number 1; _ -> Number 2

instance FromJSON Player where
  parseJSON = withScientific "" $ \case
    1 -> pure PMax
    2 -> pure PMin
    _ -> empty

                         
type Search m score state move =
     (Ord score, Monad m)
  => (Player -> state -> m [(state, move)]) -- possible moves
  -> (state -> m score)                     -- heuristic
  -> Depth                                  -- max search depth     
  -> Player                                 -- starting player
  -> state                                  -- starting state
  -> m (score, Maybe move)                  -- result score, chosen move


-- | Compute the next move given the parameters.
--   Iteratively deepens search until we either time out
--   or reach the maximum depth. Returns an arbitrary valid move
--   if it can't finish any search before the timeout.
nextMove ::
     (Ord score, Show score)
  => Bool                                    -- verbose mode
  -> (Player -> state -> IO [(state, move)]) -- next moves
  -> (state -> IO score)                     -- heuristic
  -> Search IO score state move              -- search algorithm     
  -> Int                                     -- timeout in microseconds
  -> Depth                                   -- max search depth
  -> Player                                  -- starting player
  -> state                                   -- starting state
  -> IO (Maybe move)                         -- resulting move
nextMove verbose moves heu alg timeout maxDepth p s = do

  let go d | d > maxDepth = pure Nothing
      go d = do
        (score, move) <- (liftIO $ alg moves heu d p s)
        maybe (pure ()) (partialResult . ((score, d),)) move
        go (d + 1)
  
  defaultMove  <- moves p s <&> (^? _head._2)
  computedMove <- runTimeoutWHNF timeout $ go 2

  when verbose $ 
    printf "nextMove verbose mode: score and max depth: %s\n\n"
      (show $ fst <$> computedMove)
  
  pure ((snd <$> computedMove) <|> defaultMove)
  

switch :: Player -> Player
switch = \case PMax -> PMin; _ -> PMax
            
adjustHeu :: Num score => Player -> score -> score
adjustHeu = \case PMax -> id; _ -> negate


-- | Order search by a specified search algorithm at the given depth (non-nega only!)
orderWith ::
     Depth                      
  -> Search m score state move  -- search to inform by
  -> Search m score state move  -- search to be informed
  -> Search m score state move  -- resulting search
orderWith d ordAlg alg moves heu = alg ordMoves heu where
  ordMoves p s =
        moves p s
    >>= each (\(s, m) -> (fst <$> ordAlg moves heu d (switch p) s) <&> (,(s, m)))
    <&> sortBy ((case p of PMax -> flip; _ -> id) $ comparing fst)
    <&> map snd
 
minimax :: Search m score state move
minimax moves heu = go
  where
    go d p s | d <= 0 = heu s <&> (,Nothing)
    go d p s = do
      let compute = case p of PMax -> maximumBy; _ -> minimumBy
      ms <- moves p s
      if null ms then
         heu s <&> (,Nothing)
      else do
         ms <- ms& each . _1 %%~ go (d - 1) (switch p)
         let ((score, _) , move) = compute (comparing (fst.fst)) ms
         pure (score, Just move)

alphaBeta :: Bounded score => Search m score state move
alphaBeta moves heu = go minBound maxBound                           
  where                           
    go _    _     d p s | d <= 0 = heu s <&> (,Nothing)
    go alpha beta d p s = do
      
      let (upd, cmp, startBound, bound, stop, goNext) = case p of
            PMax -> (max, (>), minBound, alpha, (beta <=), (`go` beta))
            PMin -> (min, (<), maxBound, beta, (<= alpha), go alpha)

          step (best@(bsc, _), bound) (st, mv) = do
            (sc, _) <- lift $ goNext bound (d - 1) (switch p) st
            let best'@(bsc', _) = if cmp sc bsc then (sc, Just mv) else best
                bound'          = upd bound bsc'
            if stop bound'
              then throwError best'
              else pure (best', bound')

          loop = foldM step ((startBound, Nothing), bound)
                  
      ms <- moves p s
      if null ms 
        then heu s <&> (,Nothing)
        else either id fst <$> runExceptT (loop ms)
