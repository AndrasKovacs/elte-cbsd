
{-# LANGUAGE
  LambdaCase, GeneralizedNewtypeDeriving, TypeFamilies,
  BangPatterns, TemplateHaskell, TupleSections, FlexibleContexts,
  OverloadedStrings, MultiParamTypeClasses, MonadComprehensions #-}

module CBSD.Ataxx where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Data.List
import Data.List.Split
import Data.Word
import Data.Char
import Data.Ix (range, inRange)
import Data.Function
import Data.Maybe

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector as V
import Data.Vector.Unboxed.Deriving
import Data.Aeson hiding (Result)

import CBSD.Search
import CBSD.Common


--------------------------------------------------

type Ix     = Int
type Move   = (Ix, Ix) -- Jump from-to
type GState = UV.Vector Cell

derivingUnbox "Cell"
  [t| Cell -> Word8 |]
  [| \case Empty -> 0; Filled PMax -> 1; Filled PMin -> 2; Block -> 3 |]
  [| \case 0 -> Empty; 1 -> Filled PMax; 2 -> Filled PMin; 3 -> Block |]


--------------------------------------------------

size    = 7
vecSize = size * size
sRange  = (0, size - 1)
vRange  = (0, vecSize - 1)
ix2 i j = i * size + j

(//) :: GState -> [(Int, Cell)] -> GState
(//) v upd = UV.modify (\v -> mapM_ (uncurry $ MUV.write v) upd) v

playerCells :: Player -> GState -> Int
playerCells p = length . filter (==Filled p) . UV.toList


--------------------------------------------------

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

--------------------------------------------------

result :: Player -> GState -> Result
result p s = case moves p s of
  [] -> case compare (playerCells p s) (playerCells (switch p) s) of
    GT -> Win p
    LT -> Win (switch p)
    EQ -> Draw
  _ -> Continue

--------------------------------------------------

heu :: GState -> Score
heu = UV.foldl' go 0 where
  go acc (Filled PMax) = acc + 1
  go acc (Filled PMin) = acc - 1
  go acc _             = acc

--------------------------------------------------


blocks :: [(Ix, Cell)]
blocks = map (,Block) [ix2 2 2, ix2 2 4, ix2 4 2, ix2 4 4]

start :: GState
start = empty // (p1Start ++ p2Start ++ blocks) where
  empty   = UV.fromList $ replicate vecSize Empty
  p1Start = map (,Filled PMax) [ix2 0 0, ix2 (size - 1) (size - 1)]
  p2Start = map (,Filled PMin) [ix2 (size - 1) 0, ix2 0 (size - 1)]

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


-- Messages
--------------------------------------------------
    
data HeuMsg
  = Close
  | Eval GState Player
  | EvalRe Int
  deriving (Eq, Show)

toBoardRep :: GState -> [[Cell]]
toBoardRep = chunksOf size . UV.toList

fromBoardRep :: [[Cell]] -> GState
fromBoardRep = (//blocks) . UV.fromList . join

instance ToJSON HeuMsg where
  toJSON Close = object [
    "messageType" .= String "CLOSE"
    ]
  toJSON (Eval board player) = object [
    "messageType" .= String "EVAL",
    "state" .= object [
      "board"      .= toBoardRep board,
      "nextPlayer" .= player
      ]
    ]
  toJSON (EvalRe score) = object [
    "messageType" .= String "EVAL_RE",
    "stateValue" .= score
    ]
  
instance FromJSON HeuMsg where
  parseJSON = withObject "" $ \obj -> do
    (tag :: String) <- obj .: "messageType"
    case tag of 
      "CLOSE"   -> pure Close
      "EVAL"    -> do
        state <- obj .: "state"
        Eval <$> (fromBoardRep <$> (state .: "board")) <*> state .: "nextPlayer"
      "EVAL_RE" -> EvalRe <$> obj .: "stateValue"
      _         -> empty


-- Test game
--------------------------------------------------    


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



