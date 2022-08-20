{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Matrix.Dynamic
    ( Dynamic(..)
    , withDyn
    , matrix
    , fromList
    , fromVector
    , fromColumns
    , fromRows
    , fromTriplet
    )where

import qualified Data.Matrix.Static.Sparse as S
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Generic as C
import qualified Data.Vector.Generic         as G
import Data.Kind (Type)
import Data.Singletons
import GHC.TypeLits.Singletons
import Flat (Flat(..))

data Dynamic (m :: C.MatrixKind) (v :: Type -> Type) a where
    Dynamic :: m r c v a -> Dynamic m v a

instance (G.Vector v a, Flat (v a)) => Flat (Dynamic D.Matrix v a) where
    encode (Dynamic mat@(D.Matrix vec)) = encode (r, c, vec)
      where
        (r,c) = C.dim mat
    decode = do
        (r, c, vec) <- decode
        withSomeSing (fromIntegral (r :: Int)) $ \(SNat :: Sing r) ->
            withSomeSing (fromIntegral (c :: Int)) $ \(SNat :: Sing c) ->
                return $ Dynamic (D.Matrix vec :: D.Matrix r c v a)
    size (Dynamic mat@(D.Matrix vec)) = size (r, c, vec)
      where
        (r,c) = C.dim mat

instance (G.Vector v a, S.Zero a, Flat (v a))  =>
    Flat (Dynamic S.SparseMatrix v a) where
        encode (Dynamic mat@(S.SparseMatrix nnz inner outer)) = encode (r, c, nnz, inner, outer)
          where
            (r,c) = C.dim mat
        decode = do
            (r, c, nnz, inner, outer) <- decode
            withSomeSing (fromIntegral (r :: Int)) $ \(SNat :: Sing r) ->
                withSomeSing (fromIntegral (c :: Int)) $ \(SNat :: Sing c) ->
                    return $ Dynamic (S.SparseMatrix nnz inner outer :: S.SparseMatrix r c v a)
        size (Dynamic mat@(S.SparseMatrix nnz inner outer)) = size (r, c, nnz, inner, outer)
          where
            (r,c) = C.dim mat

withDyn :: Dynamic m v a -> (forall r c. m r c v a -> b) -> b
withDyn (Dynamic x) f = f x
{-# INLINE withDyn #-}

matrix :: forall m v a. C.Matrix m v a => [[a]] -> Dynamic m v a
matrix lists = withSomeSing (fromIntegral r) $ \(SNat :: Sing r) ->
        withSomeSing (fromIntegral c) $ \(SNat :: Sing c) ->
            Dynamic (C.matrix lists :: m r c v a)
  where
    r = length lists
    c = length $ head lists
{-# INLINE matrix #-}

-- | Construct matrix from a list containg columns.
fromList :: forall v a m. C.Matrix m v a
         => (Int, Int) -> [a] -> Dynamic m v a
fromList d = fromVector d . G.fromList
{-# INLINE fromList #-}

-- | Construct matrix from a vector containg columns.
fromVector :: forall v a m. C.Matrix m v a
           => (Int, Int) -> v a -> Dynamic m v a
fromVector (r, c) vec = withSomeSing (fromIntegral r) $ \(SNat :: Sing r) ->
        withSomeSing (fromIntegral c) $ \(SNat :: Sing c) ->
            Dynamic (C.fromVector vec :: m r c v a)
{-# INLINE fromVector #-}

-- | O(m*n) Create matrix from rows
fromRows :: forall m v a. C.Matrix m v a
          => [v a] -> Dynamic m v a
fromRows xs = withSomeSing r $ \(SNat :: Sing r) -> 
    withSomeSing c $ \(SNat :: Sing c) -> Dynamic (C.fromRows xs :: m r c v a)
  where
    r = fromIntegral $ length xs
    c = fromIntegral $ G.length $ head xs
{-# INLINE fromRows #-}

fromColumns :: forall m v a. C.Matrix m v a
            => [v a] -> Dynamic m v a
fromColumns xs = withSomeSing r $ \(SNat :: Sing r) -> 
    withSomeSing c $ \(SNat :: Sing c) -> Dynamic (C.fromColumns xs :: m r c v a)
  where
    c = fromIntegral $ length xs
    r = fromIntegral $ G.length $ head xs
{-# INLINE fromColumns #-}

fromTriplet :: forall u v a. (G.Vector u (Int, Int, a), G.Vector v a)
            => (Int, Int) -> u (Int, Int, a) -> Dynamic S.SparseMatrix v a
fromTriplet (r, c) triplets = withSomeSing (fromIntegral r) $ \(SNat :: Sing r) -> 
    withSomeSing (fromIntegral c) $ \(SNat :: Sing c) ->
        Dynamic (S.fromTriplet triplets :: S.SparseMatrix r c v a)
{-# INLINE fromTriplet #-}