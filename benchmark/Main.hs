{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

import Weigh
import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Generic as G
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Singletons.Prelude hiding ((@@))
import Data.Singletons.TypeLits
import Control.Monad
import Control.DeepSeq

mat1 :: [Double] -> D.Matrix 1000 1000 U.Vector Double
mat1 = D.fromList

mat2 :: [Double] -> Matrix 1000 1000 Double
mat2 = D.fromList

mat3 :: [Double] -> D.Matrix 1000 1000 V.Vector Double
mat3 = D.fromList

main :: IO ()
main =
  let !elems = force [1 :: Double .. 1000000]
   in mainWith
      (do func "base" id elems
          func "1000 x 1000" mat1 elems
          func "1000 x 1000" mat2 elems
          func "1000 x 1000" mat3 elems )