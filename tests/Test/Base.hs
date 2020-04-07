{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Test.Base (base) where

import Test.Tasty
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons hiding ((@@))
import Data.Singletons.Prelude (Min)
import Data.Vector (Vector)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty.QuickCheck

import Test.Utils

base :: TestTree
base = testGroup "Base"
    [ pConversion
    , pTranspose
    ]

pConversion = testGroup "Conversion"
    [ testProperty "Dense: fromVector . flatten" t1
    , testProperty "Sparse: fromVector . flatten" t2
    , testProperty "Sparse -- dense" t3 ]
  where
    t1 :: D.Matrix 80 60 Vector Int -> Bool
    t1 mat = (D.fromVector $ D.flatten mat) == mat
    t2 :: S.SparseMatrix 80 60 Vector Int -> Bool
    t2 mat = mat == S.fromVector (S.flatten mat)
    t3 :: D.Matrix 80 60 Vector Int -> Bool
    t3 mat = (D.fromVector $ S.flatten mat') == mat
      where
        mat' = S.fromVector $ D.flatten mat :: S.SparseMatrix 80 60 Vector Int

pTranspose = testGroup "Transpose"
    [ testProperty "Dense" t1 ]
  where
    t1 :: D.Matrix 80 40 Vector Int -> Bool
    t1 mat = D.transpose (D.transpose mat) == mat