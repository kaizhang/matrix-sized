{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.Tasty
import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons
import Data.Vector.Storable (Storable)
import GHC.TypeNats (KnownNat)
import Control.Monad.IO.Class (liftIO)

import Test.Tasty.QuickCheck

instance (Arbitrary a, KnownNat m, KnownNat n) => Arbitrary (Matrix m n a) where
    arbitrary = D.fromList <$> vector (m*n)
      where
        m = fromIntegral $ fromSing (sing :: Sing m)
        n = fromIntegral $ fromSing (sing :: Sing n)
    shrink _v = []

{-
propTranspose :: Matrix 50 100 Double -> Bool
propTranspose m = D.transpose (D.transpose m) == m && 
    D.convertAny (S.transpose $ S.transpose (D.convertAny m)) == m
-}

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ testProperty "" square
    ]

