
module IDS (ids, Timeout(..)) where

import Control.Monad.IO.Class
import System.Timeout.Returning

ids ::
     (Num a, Ord a)
  => (player -> Int -> state -> Timeout move (score, Maybe move))
               -- tree search partially applied to "moves" and "heuristic"
  -> Int       -- timeout in microseconds
  -> Int       -- maximum depth     
  -> player    -- starting player
  -> state     -- starting state
  -> IO (Maybe move)
ids search timeout maxDepth p s = runTimeoutWHNF timeout $ go 0 where
  go d | d >= maxDepth = pure Nothing
  go d = do
    (_, move) <- search p d s
    maybe (pure ()) partialResult move
    go (d + 1)    
