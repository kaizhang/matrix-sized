{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--import Test.Tasty
import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons

{-
main :: IO ()
main = defaultMain $ testGroup "Main"
    [ 
    ]
    -}


lmat :: Matrix 5000 5000 Double
lmat = D.fromList [1..25000000]

main = do
    print $ lmat %*% lmat