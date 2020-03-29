{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Main where

import Test.Tasty
import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons hiding ((@@))
import Data.Singletons.Prelude (Min)
import Data.Complex
import Data.Vector.Storable (Storable)
import GHC.TypeNats (KnownNat)
import Control.Monad.IO.Class (liftIO)

import Test.Tasty.QuickCheck

instance (Storable a, Arbitrary a, SingI m, SingI n) => Arbitrary (Matrix m n a) where
    arbitrary = D.fromList <$> vector (m*n)
      where
        m = fromIntegral $ fromSing (sing :: Sing m)
        n = fromIntegral $ fromSing (sing :: Sing n)
    shrink _v = []

svdTest = testGroup "SVD"
    [ testProperty "" svd1
    , testProperty "" svd2
    ]
  where
    f :: ( Show a, SingI m, SingI n, p ~ Min m n, SingI p
         , Ord a, Numeric a, Fractional a) => Matrix m n a -> Bool
    f m = D.sum (m' - m) < 1
      where
        m' = u @@ S.diag d @@ D.transpose v
        (u,d,v) = svd m
    svd1 :: Matrix 50 100 Float -> Bool
    svd1 = f
    svd2 :: Matrix 50 100 Double -> Bool
    svd2 = f


propEigen :: Matrix 49 49 Double -> Bool
propEigen raw = m' @@ v == v @@ S.diag d
  where
    m = raw + D.transpose raw
    m' = D.map (\x -> mkPolar x 0) m
    (d, v)= eigs (sing :: Sing 10) m

{-
propTranspose :: Matrix 50 100 Double -> Bool
propTranspose m = D.transpose (D.transpose m) == m && 
    D.convertAny (S.transpose $ S.transpose (D.convertAny m)) == m
-}

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ svdTest
    , testProperty "" propEigen
    ]

