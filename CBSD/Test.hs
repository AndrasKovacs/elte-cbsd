{-# LANGUAGE ViewPatterns, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Function
import Data.List.Split
import Network
import System.IO
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Unboxed as UV

import qualified CBSD.Ataxx as Ax
import CBSD.Search   (nextMove, alphaBeta, orderWith, minimax, Player(..))
import CBSD.Messages (GT_Heu(..), GTH_State(..))
import CBSD.HeuMain  (heuMain, putMsg, getMsg)
import CBSD.Common
import CBSD.MessageGen

type StRep = [[Ax.Cell]]

toRep :: Ax.GState -> StRep
toRep = chunksOf Ax.size . UV.toList

fromRep :: StRep -> Ax.GState
fromRep = UV.fromList . concat

port :: PortNumber
port = 6456

heuComponent :: IO ()
heuComponent = heuMain (pure port) (_unScore . Ax.heu . fromRep)

main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  
  bracket (listenOn (PortNumber port)) sClose $ \socket -> do

    forkIO heuComponent
    printf "main: started heuComponent\n"    
    
    bracket ((^._1) <$> accept socket) hClose $ \handle -> do
      
      printf "main: accepted connection from heuComponent\n"       
      hSetBuffering handle LineBuffering
      
      let requestHeu :: Ax.GState -> IO Score
          requestHeu (toRep -> s) = do
            putMsg handle (GTH_REQ_EVAL $ GTH_State s PMax)
            Just (Wrap (GTH_RES_EVAL_RE score :: GT_Heu StRep))
              <- decodeStrict <$> B.hGetLine handle
            pure $ Score score
      
          mkSearch = nextMove True ((pure.). Ax.moves) requestHeu
          easy     = mkSearch (orderWith 0 minimax alphaBeta) (1*10^6) 2
          medium   = mkSearch (orderWith 0 minimax alphaBeta) (1*10^6) 3
          hard     = mkSearch (orderWith 0 minimax alphaBeta) (2*10^6) maxBound
      
      Ax.game hard Ax.start
  


