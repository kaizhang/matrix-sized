{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}
module Data.Matrix.Static.Dense
    (
    -- * Immutable Matrix
      Matrix(..)

    -- * Accessors
    -- ** length information
    , C.dim
    , C.rows
    , C.cols

    -- ** Query
    , (C.!)
    , C.takeRow
    , C.takeColumn
    , C.takeDiag

    -- ** Unsafe Query
    , C.unsafeIndex
    , C.unsafeTakeRow
    , C.unsafeTakeColumn

    -- * Construction
    , C.empty
    , C.matrix
    , C.fromVector
    , C.fromList
    , C.fromRows
    , C.fromColumns
    , C.unsafeFromVector
    , replicate
    , diag
    , diagRect

    -- * Conversions
    , C.flatten
    , C.toRows
    , C.toColumns
    , C.toList

    -- * Conversion between Different matrix types
    , convert
    , C.convertAny

    , C.transpose

    , C.map
    , C.mapM
    , C.imap
    , C.imapM

    -- * Zipping
    , zip
    , zip3
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , izipWith
    , izipWith3

    -- * Monadic Zipping
    , zipWithM
    , zipWithM_

    -- * Unzipping
    , unzip
    , unzip3

    , generate

    -- * Mutable matrix
    , C.thaw
    , C.unsafeThaw
    , C.freeze
    , C.unsafeFreeze
    , C.create

    , sum
    , all
    , any
    ) where

import           Control.Monad                     (liftM)
import qualified Data.Vector.Generic               as G
import Prelude hiding ( replicate, mapM, mapM_, zipWith, map
                      , sequence, sequence_, zip, unzip, zipWith3
                      , zip3, unzip3, sum, all, any )
import GHC.TypeLits (type (<=))
import Data.Singletons
import Data.Tuple (swap)
import qualified Data.List as L
import Text.Printf (printf)

import           Data.Matrix.Static.Dense.Mutable (MMatrix (..))
import qualified Data.Matrix.Static.Dense.Mutable as DM
import qualified Data.Matrix.Static.Generic as C

type instance C.Mutable Matrix = MMatrix

-- | Column-major matrix
data Matrix :: C.MatrixKind where
    Matrix :: (SingI r, SingI c) => v a -> Matrix r c v a

instance (G.Vector v a, Show a) => Show (Matrix r c v a) where
    show mat = printf "(%d x %d)\n%s" r c vals
      where
        (r,c) = C.dim mat
        vals = unlines $ L.map (unwords . L.map show . G.toList) $ C.toRows mat

instance (G.Vector v a, Eq (v a)) => Eq (Matrix r c v a) where
    (==) (Matrix v1) (Matrix v2) = v1 == v2

instance (SingI r, SingI c, G.Vector v a, Num a) =>
    Num (Matrix r c v a) where
        m1 + m2 = zipWith (+) m1 m2
        m1 - m2 = zipWith (-) m1 m2
        m1 * m2 = zipWith (*) m1 m2
        negate = C.map negate
        abs = C.map abs
        signum = undefined
        fromInteger = undefined

instance (SingI r, SingI c, G.Vector v a, Fractional a) =>
    Fractional (Matrix r c v a) where
        m1 / m2 = zipWith (/) m1 m2
        recip = C.map recip
        fromRational = undefined

instance G.Vector v a => C.Matrix Matrix v a where
    -- | O(1) Return the size of matrix.
    dim :: forall r c. Matrix r c v a -> (Int, Int)
    dim (Matrix _) = (r,c)
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE dim #-}

    -- | O(1) Unsafe indexing without bound check.
    unsafeIndex mat@(Matrix vec) (i,j) = vec `G.unsafeIndex` idx
      where
        idx = i + j * fst (C.dim mat)
    {-# INLINE unsafeIndex #-}

    -- | O(1) Create matrix from vector.
    unsafeFromVector = Matrix
    {-# INLINE unsafeFromVector #-}

    -- | O(1) Extract a row.
    unsafeTakeColumn mat@(Matrix vec) i = G.slice (i*r) r vec
      where
        (r, _) = C.dim mat
    {-# INLINE unsafeTakeColumn #-}

    -- | Create a vector by concatenating columns.
    flatten (Matrix vec) = vec
    {-# INLINE flatten #-}

    transpose mat@(Matrix vec)
        | r == 1 || c == 1 = Matrix vec
        | otherwise = Matrix $ G.generate (r*c) $ \x ->
            C.unsafeIndex mat $ x `divMod` c
      where
       (r, c) = C.dim mat
    {-# INLINE transpose #-}

    thaw (Matrix v) = MMatrix <$> G.thaw v
    {-# INLINE thaw #-}

    unsafeThaw (Matrix v) = MMatrix <$> G.unsafeThaw v
    {-# INLINE unsafeThaw #-}

    freeze (MMatrix v) = Matrix <$> G.freeze v
    {-# INLINE freeze #-}

    unsafeFreeze (MMatrix v) = Matrix <$> G.unsafeFreeze v
    {-# INLINE unsafeFreeze #-}

    map f (Matrix vec) = Matrix $ G.map f vec
    {-# INLINE map #-}

    imap f m@(Matrix vec) = Matrix $ G.imap g vec
      where
        g i = f (toIndex (C.rows m) i)
    {-# INLINE imap #-}

    imapM_ f m@(Matrix vec) = G.imapM_ g vec
      where
        g i = f (toIndex (C.rows m) i)
    {-# INLINE imapM_ #-}

    sequence (Matrix vec) = Matrix <$> G.sequence vec
    {-# INLINE sequence #-}

    sequence_ (Matrix vec) = G.sequence_ vec
    {-# INLINE sequence_ #-}

-- | O(m*n) Create a constant matrix.
replicate :: forall r c v a. (G.Vector v a, SingI r, SingI c)
          => a -> Matrix r c v a
replicate = C.unsafeFromVector . G.replicate (r*c)
  where
    r = fromIntegral $ fromSing (sing :: Sing r)
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE replicate #-}

-- | O(m*n) Create a square matrix with default values and given diagonal
diag :: (G.Vector v a, SingI n)
     => a                    -- ^ default value
     -> Matrix n 1 v a       -- ^ diagonal
     -> Matrix n n v a
diag z0 d = C.create $ do
    mat <- DM.replicate z0
    C.imapM_ (DM.unsafeWrite mat) d
    return mat
{-# INLINE diag #-}

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, SingI r, SingI c, n <= r, n <= c)
         => a                    -- ^ default value
         -> Matrix n 1 v a       -- ^ diagonal
         -> Matrix r c v a
diagRect z0 d = C.create $ do
    mat <- DM.replicate z0
    C.imapM_ (DM.unsafeWrite mat) d
    return mat
{-# INLINE diagRect #-}

zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c
           , SingI n, SingI m )
        => (a -> b -> c)
        -> Matrix n m v a -> Matrix n m v b -> Matrix n m v c
zipWith f m1 m2 = C.unsafeFromVector $ G.zipWith f (C.flatten m1) $ C.flatten m2
{-# INLINE zipWith #-}

zipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
            , SingI n, SingI m )
         => (a -> b -> c -> d)
         -> Matrix n m v a -> Matrix n m v b -> Matrix n m v c
         -> Matrix n m v d
zipWith3 f m1 m2 m3 = C.unsafeFromVector $
    G.zipWith3 f (C.flatten m1) (C.flatten m2) $ C.flatten m3
{-# INLINE zipWith3 #-}

zipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e
            , SingI n, SingI m )
         => (a -> b -> c -> d -> e)
         -> Matrix n m v a
         -> Matrix n m v b
         -> Matrix n m v c
         -> Matrix n m v d
         -> Matrix n m v e
zipWith4 f m1 m2 m3 m4 = C.unsafeFromVector $
    G.zipWith4 f (C.flatten m1) (C.flatten m2) (C.flatten m3) $ C.flatten m4
{-# INLINE zipWith4 #-}

zipWith5 :: ( G.Vector v a, G.Vector v b, G.Vector v c,G.Vector v d
            , G.Vector v e, G.Vector v f
            , SingI n, SingI m )
         => (a -> b -> c -> d -> e -> f)
         -> Matrix n m v a
         -> Matrix n m v b
         -> Matrix n m v c
         -> Matrix n m v d
         -> Matrix n m v e
         -> Matrix n m v f
zipWith5 f m1 m2 m3 m4 m5 = C.unsafeFromVector $
    G.zipWith5 f (C.flatten m1) (C.flatten m2)
    (C.flatten m3) (C.flatten m4) $ C.flatten m5
{-# INLINE zipWith5 #-}

zipWith6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
            , G.Vector v e, G.Vector v f, G.Vector v g
            , SingI n, SingI m )
         => (a -> b -> c -> d -> e -> f -> g)
         -> Matrix n m v a
         -> Matrix n m v b
         -> Matrix n m v c
         -> Matrix n m v d
         -> Matrix n m v e
         -> Matrix n m v f
         -> Matrix n m v g
zipWith6 f m1 m2 m3 m4 m5 m6 = C.unsafeFromVector $
    G.zipWith6 f (C.flatten m1) (C.flatten m2) (C.flatten m3)
    (C.flatten m4) (C.flatten m5) $ C.flatten m6
{-# INLINE zipWith6 #-}

izipWith :: ( G.Vector v a, G.Vector v b, G.Vector v c
            , SingI n, SingI m )
         => ((Int, Int) -> a -> b -> c)
         -> Matrix n m v a -> Matrix n m v b -> Matrix n m v c
izipWith f m1 m2 = C.unsafeFromVector $
    G.izipWith g (C.flatten m1) $ C.flatten m2
  where
    g i = f (toIndex (C.rows m1) i)
{-# INLINE izipWith #-}

izipWith3 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
             , SingI n, SingI m )
          => ((Int, Int) -> a -> b -> c -> d)
          -> Matrix n m v a -> Matrix n m v b -> Matrix n m v c
          -> Matrix n m v d
izipWith3 f m1 m2 m3 = C.unsafeFromVector $ G.izipWith3 g
    (C.flatten m1) (C.flatten m2) $ C.flatten m3
  where
    g i = f (toIndex (C.rows m1) i)
{-# INLINE izipWith3 #-}

zip :: (SingI n, SingI m, G.Vector v a, G.Vector v b, G.Vector v (a,b))
    => Matrix n m v a -> Matrix n m v b -> Matrix n m v (a,b)
zip m1 m2 = C.unsafeFromVector $ G.zip (C.flatten m1) $ C.flatten m2
{-# INLINE zip #-}

zip3 :: (SingI n, SingI m, G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c))
     => Matrix n m v a
     -> Matrix n m v b
     -> Matrix n m v c
     -> Matrix n m v (a,b,c)
zip3 m1 m2 m3 = C.unsafeFromVector $
    G.zip3 (C.flatten m1) (C.flatten m2) $ C.flatten m3
{-# INLINE zip3 #-}

zipWithM :: ( G.Vector v a, G.Vector v b, G.Vector v c
            , Monad monad, SingI n, SingI m )
         => (a -> b -> monad c)
         -> Matrix n m v a -> Matrix n m v b -> monad (Matrix n m v c)
zipWithM f m1 m2 = liftM C.unsafeFromVector $
    G.zipWithM f (C.flatten m1) $ C.flatten m2
{-# INLINE zipWithM #-}

zipWithM_ :: (G.Vector v a, G.Vector v b, G.Vector v c, Monad monad)
          => (a -> b -> monad c)
          -> Matrix n m v a -> Matrix n m v b -> monad ()
zipWithM_ f m1 m2 = G.zipWithM_ f (C.flatten m1) $ C.flatten m2
{-# INLINE zipWithM_ #-}

unzip :: ( G.Vector v a, G.Vector v b, G.Vector v (a,b)
         , SingI n, SingI m )
      => Matrix n m v (a,b) -> (Matrix n m v a, Matrix n m v b )
unzip m = (C.unsafeFromVector v1, C.unsafeFromVector v2)
  where
    (v1, v2) = G.unzip $ C.flatten m
{-# INLINE unzip #-}

unzip3 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c)
          , SingI n, SingI m )
       => Matrix n m v (a, b, c)
       -> (Matrix n m v a, Matrix n m v b, Matrix n m v c)
unzip3 m = (C.unsafeFromVector v1, C.unsafeFromVector v2, C.unsafeFromVector v3)
  where
    (v1, v2, v3) = G.unzip3 $ C.flatten m
{-# INLINE unzip3 #-}

generate :: forall r c v a. (G.Vector v a, SingI r, SingI c)
         => ((Int, Int) -> a) -> Matrix r c v a
generate f = C.unsafeFromVector . G.generate (r*c) $ \i -> f (i `divMod` r)
  where
    r = fromIntegral $ fromSing (sing :: Sing r)
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE generate #-}

-- | O(m*n) Convert different matrix type
convert :: (G.Vector v a, G.Vector w a) => Matrix r c v a -> Matrix r c w a
convert (Matrix vec) = Matrix $ G.convert vec
{-# INLINE convert #-}

sum :: (Num a, G.Vector v a) => Matrix r c v a -> a
sum (Matrix vec) = G.sum vec
{-# INLINE sum #-}

all :: G.Vector v a => (a -> Bool) -> Matrix r c v a -> Bool
all f (Matrix vec) = G.all f vec
{-# INLINE all #-}

any :: G.Vector v a => (a -> Bool) -> Matrix r c v a -> Bool
any f (Matrix vec) = G.any f vec
{-# INLINE any #-}

-- Helper
toIndex :: Int   -- ^ Number of rows
        -> Int   -- ^ 1-d index
        -> (Int, Int)   -- ^ 2-d index
toIndex r i = swap $ i `divMod` r
{-# INLINE toIndex #-}