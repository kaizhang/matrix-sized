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
    , fromTripletC
    , toTriplet
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
import Control.Monad.ST (runST)
import           Data.Bits                         (shiftR)
import Text.Printf (printf)
import Conduit
import Data.Conduit.Internal (zipSinks)
import Data.Tuple (swap)
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

instance (G.Vector v a, Eq (v a)) => Eq (SparseMatrix r c v a) where
    (==) (SparseMatrix a b c) (SparseMatrix a' b' c') =
        a == a' && b == b' && c == c'

instance (G.Vector v a, Zero a, Show a) => Show (SparseMatrix r c v a) where
    show mat = printf "(%d x %d)\n%s" r c vals
      where
        (r,c) = C.dim mat
        vals = unlines $ map (unwords . map show . G.toList) $ C.toRows mat

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
    unsafeFromVector vec = fromTriplet vec'
      where
        vec' = map (\((a,b),c) -> (a,b,c)) $ filter ((/=zero) . snd) $
            zipWith (\i x -> (toIndex i, x)) [0..] $ G.toList vec
        toIndex i = swap $ i `divMod` r
        r = fromIntegral $ fromSing (sing :: Sing r)
    {-# INLINE unsafeFromVector #-}

    transpose mat = runIdentity $ fromTripletC source
      where
        source = toTriplet mat .| mapC (\(i,j,x) -> (j,i,x))
    {-# INLINE transpose #-}

    thaw = undefined
    {-# INLINE thaw #-}

    unsafeThaw = undefined
    {-# INLINE unsafeThaw #-}

    freeze = undefined
    {-# INLINE freeze #-}

    unsafeFreeze = undefined
    {-# INLINE unsafeFreeze #-}

    map f (SparseMatrix vec inner outer) = SparseMatrix (G.map f vec) inner outer
    {-# INLINE map #-}

    imap f mat@(SparseMatrix _ inner outer) = SparseMatrix vec' inner outer
      where
        vec' = runST $ runConduit $ toTriplet mat .| mapC g .| sinkVector
        g (i,j,x) = f (i,j) x
    {-# INLINE imap #-}
    

-- | O(n) Create matrix from triplet. row and column indices *are not* assumed to be ordered
-- duplicate entries are carried over to the CSR represention
fromTriplet :: forall t r c v a. (Traversable t, G.Vector v a, SingI r, SingI c)
            => t (Int, Int, a) -> SparseMatrix r c v a
fromTriplet triplets = SparseMatrix val inner outer
  where
    outer = S.scanl (+) 0 $ S.create $ do
        vec <- SM.replicate c 0
        _ <- flip mapM triplets $ \(_, j, _) -> 
            SM.unsafeModify vec (+1) j
        return vec
    (val, inner) = runST $ do
        outer' <- S.thaw outer
        val' <- GM.new nnz
        inner' <- SM.new nnz
        _ <- flip mapM triplets $ \(i, j, v) -> do
            idx <- fromIntegral <$> SM.unsafeRead outer' j
            GM.unsafeWrite val' idx v
            SM.unsafeWrite inner' idx $ fromIntegral i
            SM.unsafeModify outer' (+1) j
        (,) <$> G.unsafeFreeze val' <*> S.unsafeFreeze inner'
    nnz = length triplets
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE fromTriplet #-}

-- | O(n) Create matrix from triplet. row and column indices *are not* assumed to be ordered
-- duplicate entries are carried over to the CSR represention
fromTripletC :: forall m r c v a. (Monad m, G.Vector v a, SingI r, SingI c)
             => ConduitT () (Int, Int, a) m ()
             -> m (SparseMatrix r c v a)
fromTripletC triplets = do
    (nnz, outer) <- runConduit $ triplets .| zipSinks lengthC sinkOuter
    (val, inner, _) <- runConduit $ triplets .| sinkValInner nnz (clone outer)
    return $ SparseMatrix val inner outer
  where
    sinkOuter = S.scanl (+) 0 <$> foldlC f (S.replicate c 0)
      where
        f vec (_, j, _) = S.modify (\v -> SM.unsafeModify v (+1) j) vec
    sinkValInner nnz outer0 = foldlC f (val0, inner0, outer0)
      where
        val0 = G.create $ GM.new nnz
        inner0 = S.create $ SM.new nnz
        f (val, inner, outer) (i, j, v) = (val', inner', outer')
          where
            idx = fromIntegral $ outer `S.unsafeIndex` j
            val' = G.create $ do
                vec <- G.unsafeThaw val
                GM.unsafeWrite vec idx v
                return vec
            inner' = S.create $ do
                vec <- S.unsafeThaw inner
                SM.unsafeWrite vec idx $ fromIntegral i
                return vec
            outer' = S.create $ do
                vec <- S.unsafeThaw outer
                SM.unsafeModify vec (+1) j
                return vec
    c = fromIntegral $ fromSing (sing :: Sing c)
    clone x = S.create $ S.thaw x
{-# INLINE fromTripletC #-}

toTriplet :: (Monad m, G.Vector v a, SingI r, SingI c)
          => SparseMatrix r c v a -> ConduitT i (Int, Int, a) m ()
toTriplet (SparseMatrix val inner outer) =
    G.ifoldM_ go (fromIntegral $ G.head outer) outer
  where
    go start curC end = do
        enumFromToC start (end'-1) .| mapC f
        return end'
      where
        end' = fromIntegral end
        f i = (fromIntegral $ inner `G.unsafeIndex` i, fromIntegral curC - 1, val `G.unsafeIndex` i)
{-# INLINE toTriplet #-}

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


-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

--getIndex :: Int -> (Int, Int)
--getIndex = 
--{-# INLINE getIndex #-}