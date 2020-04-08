{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Test.LinearAlgebra (linearAlgebra) where

import Test.Tasty
import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Generic as G
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons hiding ((@@))
import Data.Singletons.Prelude (Min)
import Data.Complex
import Data.Vector.Storable (Storable)
import GHC.TypeNats (KnownNat)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty.QuickCheck

import Test.Utils

linearAlgebra :: TestTree
linearAlgebra = testGroup "Linear algebra"
    [ svdTest
    , eigenTest
    ]

svdTest = testGroup "SVD"
    [ testProperty "SVD (Float)" svd1
    , testProperty "SVD (Double)" svd2
    ]
  where
    svd1 :: Matrix 50 30 Float -> Bool
    svd1 m = m' ~= m
      where
        m' = u @@ S.diag d @@ D.transpose v
        (u,d,v) = svd m
    svd2 :: Matrix 50 30 Double -> Bool
    svd2 m = m' ~= m
      where
        m' = u @@ S.diag d @@ D.transpose v
        (u,d,v) = svd m

eigenTest = testGroup "Eigendecomposition"
    [ testProperty "Full" eigen1
    , testProperty "Partial dense" eigen2
    , testProperty "Partial symmetric dense" eigen3
    , testProperty "Partial symmetric sparse" eigen4
    ]
  where
    eigen1 :: Matrix 100 100 Double -> Bool
    eigen1 m = (m' @@ v) ~= (v @@ S.diag d)
      where
        m' = D.map (\x -> mkPolar x 0) m
        (d, v)= eig m
    eigen2 :: Matrix 10 10 Double -> Bool
    eigen2 m = m' @@ v ~= v @@ S.diag d
      where
        m' = D.map (\x -> mkPolar x 0) m
        (d, v)= eigS (sing :: Sing 8) m
    eigen3 :: Matrix 100 100 Double -> Bool
    eigen3 raw = m @@ v ~= v @@ S.diag d
      where
        m = raw %+% D.transpose raw
        (d, v)= eigSH (sing :: Sing 99) m
    eigen4 :: SparseMatrix 100 100 Double -> Bool
    eigen4 raw = m @@ v ~= v @@ S.diag d
      where
        m = raw %+% D.transpose raw
        (d, v)= eigSH (sing :: Sing 99) m



{-
propTranspose :: Matrix 50 100 Double -> Bool
propTranspose m = D.transpose (D.transpose m) == m && 
    D.convertAny (S.transpose $ S.transpose (D.convertAny m)) == m
-}