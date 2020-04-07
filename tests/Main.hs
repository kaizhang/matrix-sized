module Main where

import Test.Tasty

import Test.Base
import Test.LinearAlgebra

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ base
    , linearAlgebra
    ]

