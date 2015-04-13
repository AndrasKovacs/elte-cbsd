{-# LANGUAGE
  LambdaCase, GeneralizedNewtypeDeriving,
  BangPatterns, TemplateHaskell #-}

module Ataxx where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.Word
import Data.Ix (range, inRange)
import Data.Function

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector as V
import Data.Vector.Unboxed.Deriving

import Search

--------------------------------------------------

data Cell     = Empty | Filled Player | Block deriving (Eq, Show)
data Result   = Win Player | Draw | Continue deriving (Eq, Show)
newtype Score = Score Int deriving (Eq, Show, Ord, Num)
type Ix       = Int
type Move     = (Ix, Ix) -- Jump from-to

instance Bounded Score where
  maxBound = Score (maxBound - 1)
  minBound = Score (minBound + 2)

--------------------------------------------------

type GState = UV.Vector Cell

derivingUnbox "Cell"
  [t| Cell -> Word8 |]
  [| \case Empty -> 0; Filled PMax -> 1; Filled PMin -> 2; Block -> 3 |]
  [| \case 0 -> Empty; 1 -> Filled PMax; 2 -> Filled PMin; 3 -> Block |]

size    = 7
vecSize = size * size
sRange  = (0, size - 1)
vRange  = (0, vecSize - 1)

(//) :: GState -> [(Int, Cell)] -> GState
(//) v upd = UV.modify (\v -> mapM_ (uncurry $ MUV.write v) upd) v


--------------------------------------------------

-- | Moore neighborhoods
mooreN :: V.Vector [Ix]
mooreN = do
  i <- V.fromList $ range sRange
  j <- V.fromList $ range sRange
  let mn = tail $ liftA2 (,) [i, i - 1, i + 1] [j, j - 1, j + 1]
      mn' = [i * size + j | (i, j) <- mn, inRange sRange i, inRange sRange j]
  pure mn'

-- | Double step neighborhoods
doubleN :: V.Vector [Ix]
doubleN = do
  i <- V.fromList $ range sRange
  j <- V.fromList $ range sRange
  let dn = delete (i, j) $ liftA2 (,) [i-2 ..  i+2] [j-2 .. j+2]
      dn' = [ix | (i', j') <- dn,
             inRange sRange i', inRange sRange j',
             let ix = i' * size + j',
             notElem ix $ mooreN V.! (i * size + j)]                                      
  pure dn'


moves :: Player -> GState -> [(GState, Move)]
moves p s = singleStep ++ doubleStep where
  
  ourUnits   = filter ((== Filled p) . (s UV.!)) $ range vRange
  convert to = filter ((== Filled (switch p)) . (s UV.!)) (mooreN V.! to)
  
  singleStep = do
    from <- ourUnits
    to   <- filter ((==Empty) . (s UV.!)) (mooreN V.! from)
    pure (s // map (,Filled p) (to : convert to), (from, to))

  doubleStep = do
    from <- ourUnits
    to   <- filter ((==Empty) . (s UV.!)) (doubleN V.! from)
    pure (s // ((from, Empty) : map (,Filled p) (to : convert to)), (from, to))

--------------------------------------------------

heu :: GState -> Score
heu = UV.foldl' go 0 where
  go acc (Filled PMax) = acc + 1
  go acc (Filled PMin) = acc - 1
  go acc _             = acc

--------------------------------------------------

start :: GState
start = _


nextMoveAtaxx' = nextMove True ((pure.).moves) (pure.heu)

nextMoveAtaxx :: Player -> GState -> IO (Maybe Move)
nextMoveAtaxx = nextMoveAtaxx' (orderWith 0 minimax alphaBeta) (1*10^6) maxBound

