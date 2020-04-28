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
import qualified Data.Matrix.Static.Generic as G
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons hiding ((@@))
import Data.Singletons.Prelude (Min)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.IO.Class (liftIO)
import Test.Tasty.QuickCheck
import Conduit
import Data.Store

import Test.Utils

base :: TestTree
base = testGroup "Base"
    [ pConversion
    , pTranspose
    , pSerialization
    ]

pSerialization :: TestTree
pSerialization = testGroup "Serialization"
    [ testProperty "Sparse: id == decode . encode" tStore ]
  where
    tStore :: S.SparseMatrix 80 60 Vector Double -> Bool
    tStore mat = mat == decodeEx (encode mat)

pConversion :: TestTree
pConversion = testGroup "Conversion"
    [ testProperty "Dense: id == fromVector . flatten" t1
    , testProperty "Sparse: id == fromVector . flatten" t2
    , testProperty "Sparse -- id == fromTriplet . toTriplet" tTri
    , testProperty "Sparse -- id == fromTripletC . toTriplet" tTriC
    , testProperty "Sparse -- id == fromColumn . toColumn" tCol
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
    tTri :: S.SparseMatrix 80 60 Vector Int -> Bool
    tTri mat = S.fromTriplet xs == mat
      where
        xs = V.fromList $ runIdentity $ runConduit $ S.toTriplet mat .| sinkList
    tTriC :: S.SparseMatrix 80 60 Vector Int -> Bool
    tTriC mat = mat == runIdentity (S.fromTripletC (S.toTriplet mat))
    tCol :: S.SparseMatrix 80 60 Vector Int -> Bool
    tCol mat = mat == G.fromColumns (map (G.unsafeTakeColumn mat) [0..G.cols mat -1])

pTranspose :: TestTree
pTranspose = testGroup "Transpose"
    [ testProperty "Dense" t1
    , testProperty "Sparse" tSp
    ]
  where
    t1 :: D.Matrix 80 40 Vector Int -> Bool
    t1 mat = D.transpose (D.transpose mat) == mat
    tSp :: S.SparseMatrix 80 40 Vector Int -> Bool
    tSp mat = G.transpose (G.transpose mat) == mat