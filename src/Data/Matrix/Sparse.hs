{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Matrix.Sparse
   ( -- * Sparse matrix
     SparseMatrix(..)

   , Zero(..)

    -- * Accessors
    -- ** length information
    , C.dim
    , C.rows
    , C.cols

    -- ** Query
    , (C.!)
    , C.takeDiag

    -- ** Unsafe Query
    , C.unsafeIndex
    , C.unsafeTakeRow
    , C.unsafeTakeColumn

    -- * Construction
    , C.empty
    , C.fromVector
    , C.fromList
    , C.unsafeFromVector

    , diag
    , diagRect

    -- * Conversions
    , C.flatten
    , C.toList

    -- * Different matrix types
    , C.convertAny
   ) where

import           Control.DeepSeq
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Singletons
import Control.Monad
import           Data.Bits                         (shiftR)
import Text.Printf (printf)
import GHC.TypeLits (type (<=))
import Foreign.C.Types
import Data.Complex

import qualified Data.Matrix.Dense as D
import qualified Data.Matrix.Internal.Class as C
import Data.Matrix.Sparse.Mutable

type instance C.Mutable SparseMatrix = MSparseMatrix

class Eq a => Zero a where
    zero :: a

instance Zero Int where
    zero = 0

instance Zero Float where
    zero = 0.0

instance Zero Double where
    zero = 0.0

instance Zero (Complex Float) where
    zero = 0

instance Zero (Complex Double) where
    zero = 0

instance Eq a => Zero ([] a) where
    zero = []

-- | Column-major mutable matrix.
data SparseMatrix :: C.MatrixKind where
    SparseMatrix :: (SingI r, SingI c)
                 => !(v a)           -- ^ Values: stores the coefficient values
                                     -- of the non-zeros.
                 -> !(S.Vector CInt)  -- ^ InnerIndices: stores the row
                                     -- (resp. column) indices of the non-zeros.
                 -> !(S.Vector CInt)  -- ^ OuterStarts: stores for each column
                                     -- (resp. row) the index of the first
                                     -- non-zero in the previous two arrays.
                 -> SparseMatrix r c v a

instance (G.Vector v a, Zero a, Show a) => Show (SparseMatrix r c v a) where
    show mat = printf "(%d x %d)\n%s" r c vals
      where
        (r,c) = C.dim mat
        vals = unlines $ map (unwords . map show . G.toList) $ C.toRows mat

instance (NFData (v a)) => NFData (SparseMatrix r c v a) where
    rnf (SparseMatrix vec inner outer) = rnf vec

instance (G.Vector v a, Zero a) => C.Matrix SparseMatrix v a where
    -- | O(1) Return the size of matrix.
    dim :: forall r c. SparseMatrix r c v a -> (Int, Int)
    dim (SparseMatrix _ _ _) = (r,c)
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE dim #-}

    -- | O(1) Unsafe indexing without bound check.
    unsafeIndex (SparseMatrix vec inner outer) (i,j) = 
        case binarySearchByBounds inner (fromIntegral i) r0 r1 of
            Nothing -> zero
            Just k -> vec `G.unsafeIndex` k
      where
        r0 = fromIntegral $ outer `S.unsafeIndex` j
        r1 = fromIntegral $ outer `S.unsafeIndex` (j+1) - 1
    {-# INLINE unsafeIndex #-}

    -- | O(1) Create matrix from vector containing columns.
    unsafeFromVector :: forall r c. (G.Vector v a, SingI r, SingI c)
           => v a -> SparseMatrix r c v a
    unsafeFromVector vec = SparseMatrix
        (G.generate n (G.unsafeIndex vec . S.unsafeIndex nz))
        inner outer
      where
        inner = S.map fromIntegral $ S.map (`mod` c) nz
        outer = S.create $ do
            v <- SM.replicate (c+1) 0
            S.forM_ nz $ \x -> do
                let i = x `div` r
                SM.unsafeModify v succ (i+1)
            forM_ [1..c] $ \i -> do
                x <- SM.unsafeRead v (i-1)
                SM.unsafeModify v (+x) i
            return v
        nz = S.filter (\i -> vec `G.unsafeIndex` i /= zero) $ S.enumFromN 0 (r*c)
        n = S.length nz
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE unsafeFromVector #-}

    thaw = undefined
    {-# INLINE thaw #-}

    unsafeThaw = undefined
    {-# INLINE unsafeThaw #-}

    freeze = undefined
    {-# INLINE freeze #-}

    unsafeFreeze = undefined
    {-# INLINE unsafeFreeze #-}

    map f (SparseMatrix vec inner outer) = SparseMatrix (G.map f vec) inner outer
    imap = undefined
    {-# INLINE map #-}

-- | O(m*n) Create a square matrix with given diagonal.
diag :: (G.Vector v a, Zero a, SingI n)
     => D.Matrix n 1 v a       -- ^ diagonal
     -> SparseMatrix n n v a
diag = diagRect
{-# INLINE diag #-}

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, Zero a, SingI r, SingI c, n <= r, n <= c)
         => D.Matrix n 1 v a       -- ^ diagonal
         -> SparseMatrix r c v a
diagRect d = SparseMatrix (C.flatten d) (S.enumFromN 0 n) (S.enumFromN 0 $ n + 1)
  where
    n = C.rows d
{-# INLINE diagRect #-}

binarySearchByBounds :: S.Vector CInt -> CInt -> Int -> Int -> Maybe Int
binarySearchByBounds vec x = loop
  where
    loop !l !u
        | l > u = Nothing
        | x == x' = Just k
        | x < x' = loop l (k-1)
        | otherwise = loop (k+1) u
      where
        k = (u+l) `shiftR` 1
        x' = vec `S.unsafeIndex` k
{-# INLINE binarySearchByBounds #-}