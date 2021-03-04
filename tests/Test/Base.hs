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
import qualified Data.Matrix.Dynamic as Dyn
import Control.Monad.ST (runST)
import Data.Matrix.Static.IO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Tasty.QuickCheck
import Conduit
import Flat (flat, unflat)
import Flat.Instances.Vector ()
import Data.Either

import Test.Utils ()

base :: TestTree
base = testGroup "Base"
    [ pConversion
    , pTranspose
    , pSerialization
    ]

pSerialization :: TestTree
pSerialization = testGroup "Serialization"
    [ testProperty "Dense: id == decode . encode" tStoreD
    , testProperty "Dense: id == decode . encode" tStoreD'
    , testProperty "Sparse: id == decode . encode" tStoreS
    , testProperty "Sparse: id == decode . encode" tStoreS'
    , testProperty "Sparse: id ~= fromMM . toMM" tMM ]
  where
    tStoreD :: D.Matrix 80 60 Vector Double -> Bool
    tStoreD mat = mat == fromRight undefined (unflat $ flat mat)
    tStoreD' :: D.Matrix 80 60 Vector Double -> Bool
    tStoreD' mat = G.flatten mat == Dyn.withDyn
        (either (error . show) id $ unflat $ flat mat :: Dyn.Dynamic D.Matrix Vector Double) G.flatten
    tStoreS :: S.SparseMatrix 80 60 Vector Double -> Bool
    tStoreS mat = mat == fromRight undefined (unflat $ flat mat)
    tStoreS' :: S.SparseMatrix 80 60 Vector Double -> Bool
    tStoreS' mat = G.flatten mat == Dyn.withDyn
        (fromRight undefined $ unflat $ flat mat :: Dyn.Dynamic S.SparseMatrix Vector Double) G.flatten
    tMM :: S.SparseMatrix 80 60 Vector Int -> Bool
    tMM mat = runST (runConduit $ toMM mat .| fromMM) == mat

pConversion :: TestTree
pConversion = testGroup "Conversion"
    [ testProperty "Dense: id == fromVector . flatten" t1
    , testProperty "Sparse: id == fromVector . flatten" t2
    , testProperty "Sparse -- id == fromTriplet . toTriplet" tTri
    , testProperty "Sparse -- id == fromTripletC . toTriplet" tTriC
    , testProperty "Sparse -- id == fromColumn . toColumn" tCol
    , testProperty "Sparse -- toTripletC == mapM_ takeColumnC" tCol'
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
    tCol' :: S.SparseMatrix 80 60 Vector Int -> Bool
    tCol' mat = a == b
      where
        a = runIdentity $ runConduit $
            mapM_ (S.unsafeTakeColumnC mat) [0..G.cols mat -1] .| sinkList
        b = runIdentity $ runConduit $ S.toTriplet mat .| sinkList

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