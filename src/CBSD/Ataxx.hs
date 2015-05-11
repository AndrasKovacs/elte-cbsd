
{-# LANGUAGE
  OverloadedStrings,
  LambdaCase, TemplateHaskell, TupleSections,
  MultiParamTypeClasses, TypeFamilies,
  ScopedTypeVariables #-}

module CBSD.Ataxx where

import CBSD.Search

import Control.Lens hiding ((.=), coerce)
import Data.Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Word
import Data.Ix (range, inRange)
import Data.Coerce

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector as V
import           Data.Vector.Unboxed.Deriving

{-
    Position of blocks is hard-wired!!
    Also, We disallow blocks when converting to/from JSON!
-}

-- Types
------------------------------------------------------------

data Cell   = Filled Player | Empty | Block deriving (Eq, Show)
type Ix     = Int
type Move   = (Ix, Ix) -- Jump from-to
type GState = UV.Vector Cell

derivingUnbox "Cell"
  [t| Cell -> Word8 |]
  [| \case Empty -> 0; Filled PMax -> 1; Filled PMin -> 2; Block -> 3 |]
  [| \case 0 -> Empty; 1 -> Filled PMax; 2 -> Filled PMin; 3 -> Block |] 

-- Constants + Helpers
------------------------------------------------------------

size    = 7
vecSize = size * size
sRange  = (0, size - 1)
vRange  = (0, vecSize - 1)
ix2 i j = i * size + j
unIx2 i = divMod i size

(//) :: GState -> [(Int, Cell)] -> GState
(//) v upd = UV.modify (\v -> mapM_ (uncurry $ MUV.write v) upd) v

playerCells :: Player -> GState -> Int
playerCells p = length . filter (==Filled p) . UV.toList


-- Moves
------------------------------------------------------------

-- | Single step neighborhoods
singleN :: V.Vector [Ix]
singleN = do
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
             notElem ix $ singleN V.! (i * size + j)]                                      
  pure dn'


moves :: Player -> GState -> [(GState, Move)]
moves p s = singleStep ++ doubleStep where
  
  ourUnits   = filter ((== Filled p) . (s UV.!)) $ range vRange
  convert to = filter ((== Filled (switch p)) . (s UV.!)) (singleN V.! to)
  
  singleStep = do
    from <- ourUnits
    to   <- filter ((==Empty) . (s UV.!)) (singleN V.! from)
    pure (s // map (,Filled p) (to : convert to), (from, to))

  doubleStep = do
    from <- ourUnits
    to   <- filter ((==Empty) . (s UV.!)) (doubleN V.! from)
    pure (s // ((from, Empty) : map (,Filled p) (to : convert to)), (from, to))

makeMove :: Player -> GState -> Move -> Maybe GState
makeMove p s m = fst <$> (find ((==m).snd) $ moves p s)

publicMakeMove :: Player -> GStateJSON -> MoveJSON -> Maybe GStateJSON
publicMakeMove = coerce makeMove

publicMoves :: Player -> GStateJSON -> [MoveJSON]
publicMoves p s = coerce (map snd $ moves p (coerce s))

-- Heuristic
------------------------------------------------------------

result :: Player -> GState -> Result
result p s = case moves p s of
  [] -> case compare (playerCells p s) (playerCells (switch p) s) of
    GT -> Win p
    LT -> Win (switch p)
    EQ -> Draw
  _ -> Continue

heu :: GState -> Score
heu = UV.foldl' go 0 where
  go acc (Filled PMax) = acc + 1
  go acc (Filled PMin) = acc - 1
  go acc _             = acc

publicHeu :: GStateJSON -> Player -> Int
publicHeu = coerce (\s (_ :: Player) -> heu s)

-- Start state
------------------------------------------------------------

blocks :: [(Ix, Cell)]
blocks = map (,Block) [ix2 2 2, ix2 2 4, ix2 4 2, ix2 4 4]

start :: GState
start = empty // (p1Start ++ p2Start ++ blocks) where
  empty   = UV.fromList $ replicate vecSize Empty
  p1Start = map (,Filled PMax) [ix2 0 0, ix2 (size - 1) (size - 1)]
  p2Start = map (,Filled PMin) [ix2 (size - 1) 0, ix2 0 (size - 1)]

publicStart :: GStateJSON
publicStart = coerce start

-- Serialization
------------------------------------------------------------

instance FromJSON Cell where
  parseJSON = withScientific "" $ \case
    0 -> pure Empty
    1 -> pure $ Filled PMax
    2 -> pure $ Filled PMin
    _ -> empty

instance ToJSON Cell where
  toJSON = \case
    Empty       -> Number 0
    Filled PMax -> Number 1
    Filled PMin -> Number 2
    Block       -> Number 0

newtype MoveJSON = MoveJSON Move deriving (Eq, Show)

instance ToJSON MoveJSON where
  toJSON (MoveJSON (from, to)) = object
    ["from" .= object ["y" .= y1, "x" .= x1],
     "to"   .= object ["y" .= y2, "x" .= x2]]
    where (y1, x1) = unIx2 from
          (y2, x2) = unIx2 to

instance FromJSON MoveJSON where
  parseJSON = withObject "" $ \obj -> do
    from <- obj .: "from"
    to   <- obj .: "to"
    MoveJSON <$> ((,)
      <$> (ix2 <$> (from .: "y") <*> (from .: "x"))
      <*> (ix2 <$> (to   .: "y") <*> (to   .: "x")))
        
newtype GStateJSON = GStateJSON GState deriving (Eq, Show)

instance ToJSON GStateJSON where
  toJSON (GStateJSON v) =
    toJSON $ chunksOf size $ map (\case Block -> Empty; x -> x) $ UV.toList v

instance FromJSON GStateJSON where
  parseJSON val =
    GStateJSON . (//blocks) . UV.fromList . concat <$> (parseJSON val :: Parser [[Cell]])
