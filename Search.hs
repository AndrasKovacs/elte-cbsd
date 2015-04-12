
{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns, RankNTypes, FlexibleContexts #-}

module Search (
    Player(..)
  , TreeSearch
  , minimax
  , negamax
  , alphaBeta
  , negaAlphaBeta ) where


import Control.Applicative
import Control.Lens
import Data.List
import Data.Ord
import Control.Monad.Except
 
data Player = PMax | PMin deriving (Eq, Show)
                         
switch :: Player -> Player
switch = \case PMax -> PMin; _ -> PMax
            
adjustHeu :: Num score => Player -> score -> score
adjustHeu = \case PMax -> id; _ -> negate

{-
  CAUTION : Only use negaX searches if the all of the following holds:

    1. negate minBound == maxBound
    2. negate maxBound == minBound
    3. (heuristic of a state with PMax) == negate (heuristic of state with PMin)

  Example for violation: (negate minBound :: Int) /= maxBound
  Use newtype wrappers when necessary to ensure the above conditions.
-} 
 
 
type TreeSearch m score state move =
     (Ord score, Monad m)
  => (Player -> state -> m [(state, move)]) -- possible moves
  -> (state -> m score)                     -- heuristic
  -> Player                                 -- starting player
  -> Int                                    -- max search depth
  -> state                                  -- starting state
  -> m (score, Maybe move)                  -- result score, chosen move
  
 
minimax :: TreeSearch m score state move
minimax moves heu = go
  where
    go p 0 s = heu s <&> (,Nothing)
    go p d s = do
      let compute = case p of PMax -> maximumBy; _ -> minimumBy
      ms <- moves p s
      if null ms then
         heu s <&> (,Nothing)
      else do
         ms <- ms& each . _1 %%~ go (switch p) (d - 1)
         let ((score, _) , move) = compute (comparing (fst.fst)) ms
         pure (score, Just move)


negamax :: Num score => TreeSearch m score state move
negamax moves heu = go
  where    
    go p 0 s = (adjustHeu p <$> heu s) <&> (,Nothing)
    go p d s = do
      ms <- moves p s
      if null ms then
         (adjustHeu p <$> heu s) <&> (,Nothing)
      else do
         ms <- ms& each . _1 %%~ \st ->
                 go (switch p) (d - 1) st <&> _1 %~ negate
         let ((score, _) , move) = maximumBy (comparing (fst.fst)) ms
         pure (score, Just move)


alphaBeta :: Bounded score => TreeSearch m score state move
alphaBeta moves heu = go minBound maxBound                           
  where                           
    go _    _     p 0 s = heu s <&> (,Nothing)
    go alpha beta p d s = do
      
      let (upd, cmp, startBound, bound, stop, goNext) = case p of
            PMax -> (max, (>), minBound, alpha, (beta <=), (`go` beta))
            PMin -> (min, (<), maxBound, beta, (<= alpha), go alpha)

          step (best@(bsc, _), bound) (st, mv) = do
            (sc, _) <- lift $ goNext bound (switch p) (d - 1) st
            let best'@(bsc', _) = if cmp sc bsc then (sc, Just mv) else best
                bound'          = upd bound bsc'
            if stop bound'
              then throwError best'
              else pure (best', bound')

          loop = foldM step ((startBound, Nothing), bound)
                  
      ms <- moves p s      
      if null ms then
        heu s <&> (,Nothing)
      else
        either id fst <$> runExceptT (loop ms)


negaAlphaBeta :: (Bounded score, Num score) => TreeSearch m score state move
negaAlphaBeta moves heu = go minBound maxBound                           
  where                           
    go _    _     p 0 s = (adjustHeu p <$> heu s) <&> (,Nothing)
    go alpha beta p d s = do

      let step (best@(bsc, _), alpha) (st, mv) = do
            (negate -> sc, _) <- lift $ go (-beta) (-alpha) (switch p) (d - 1) st            
            let best'@(bsc', _) = if sc > bsc then (sc, Just mv) else best
                alpha' = max alpha bsc'                
            if beta <= alpha'
              then throwError best'
              else pure (best', alpha')              
            
          loop = foldM step ((minBound, Nothing), alpha)
                  
      ms <- moves p s      
      if null ms
        then (adjustHeu p <$> heu s) <&> (,Nothing)      
        else either id fst <$> runExceptT (loop ms)
