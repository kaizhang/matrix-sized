{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Matrix.Static.Sparse
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
    , fromTriplet
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

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Singletons
import Control.Monad
import Control.Monad.ST (runST)
import           Data.Bits                         (shiftR)
import Text.Printf (printf)
import GHC.TypeLits (type (<=))
import Foreign.C.Types
import Data.Complex

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Generic as C
import Data.Matrix.Static.Sparse.Mutable

type instance C.Mutable SparseMatrix = MSparseMatrix

class Eq a => Zero a where
    zero :: a

instance Zero Int where
    zero = 0

instance Zero Float where
    zero = 0.0

instance Zero CFloat where
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
                 => (v a)           -- ^ Values: stores the coefficient values
                                     -- of the non-zeros.
                 -> (S.Vector CInt)  -- ^ InnerIndices: stores the row
                                     -- (resp. column) indices of the non-zeros.
                 -> (S.Vector CInt)  -- ^ OuterStarts: stores for each column
                                     -- (resp. row) the index of the first
                                     -- non-zero in the previous two arrays.
                 -> SparseMatrix r c v a

instance (G.Vector v a, Zero a, Show a) => Show (SparseMatrix r c v a) where
    show mat = printf "(%d x %d)\n%s" r c vals
      where
        (r,c) = C.dim mat
        vals = unlines $ map (unwords . map show . G.toList) $ C.toRows mat

instance (SingI r, SingI c, G.Vector v a, Zero a, Num a) =>
    Num (SparseMatrix r c v a) where
        m1 + m2 = undefined
        m1 - m2 = undefined
        m1 * m2 = undefined
        negate = C.map negate
        abs = C.map abs
        signum = undefined
        fromInteger = undefined

instance (SingI r, SingI c, G.Vector v a, Zero a, Fractional a) =>
    Fractional (SparseMatrix r c v a) where
        m1 / m2 = undefined
        recip = C.map recip
        fromRational = undefined

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

    transpose (SparseMatrix val inner outer) = undefined 

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

-- | O(n) Create matrix from triplet. row and column indices *are not* assumed to be ordered
-- duplicate entries are carried over to the CSR represention
fromTriplet :: forall t r c v a. (Traversable t, G.Vector v a, SingI r, SingI c)
            => t (Int, Int, a) -> SparseMatrix r c v a
fromTriplet triplets = SparseMatrix val inner outer
  where
    outer = S.scanl (+) 0 $ S.create $ do
        vec <- SM.replicate c 0
        _ <- flip mapM triplets $ \(_, j, _) -> 
            SM.modify vec (+1) j
        return vec
    (val, inner) = runST $ do
        outer' <- S.thaw outer
        val' <- GM.new nnz
        inner' <- SM.new nnz
        _ <- flip mapM triplets $ \(i, j, v) -> do
            idx <- fromIntegral <$> SM.read outer' j
            GM.write val' idx v
            SM.write inner' idx $ fromIntegral i
            SM.modify outer' (+1) j
        (,) <$> G.unsafeFreeze val' <*> S.unsafeFreeze inner'
    nnz = length triplets
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE fromTriplet #-}

{-
toTriplet :: (G.Vector v1 (Int, Int, a), G.Vector v2 a, SingI r, SingI c)
          => SparseMatrix r c v2 a -> v1 (Int, Int, a)
toTriplet mat = 
-}

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
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
