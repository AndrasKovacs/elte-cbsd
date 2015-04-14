

{-
TODO

Szeretnénk olyan random állapotot generálni, ami
helyes lépések sorozataként áll elő.

A modul célja az lenne, hogy az alternatív "moves" implementációkat
QC-vel össze tudjuk vetni a "referencia" verzióval.
-}


{-# LANGUAGE TupleSections #-}

module CBSD.PotyogosTest where

import Control.Applicative
import Test.QuickCheck

import qualified CBSD.Potyogos as P
import qualified CBSD.Search as S

genMove :: Gen P.Move
genMove = choose (1, P.cols)








