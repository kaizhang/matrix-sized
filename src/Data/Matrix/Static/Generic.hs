{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}
module Data.Matrix.Static.Generic
    ( Mutable
    , Matrix(..)
    , MatrixKind

    -- * Matrix query
    , rows
    , cols
    , (!)
    , takeColumn
    , takeRow
    , toRows
    , toColumns

    -- * Matrix Construction
    , empty
    , matrix
    , fromRows
    , fromColumns
    , fromVector
    , fromList

    , toList
    , create
    , convertAny
    , mapM
    , imapM
    ) where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (ST, runST)
import qualified Data.Vector.Generic         as G
import Text.Printf (printf)
import Prelude hiding (map, mapM, mapM_, sequence, sequence_)
import qualified Data.List as L
import Data.Tuple (swap)
import Data.Kind (Type)
import GHC.TypeLits (Nat, type (<=))
import Data.Singletons (SingI, Sing, fromSing, sing, withSomeSing)
import Data.Singletons.TypeLits

import Data.Matrix.Static.Generic.Mutable (MMatrix, MMatrixKind)

type MatrixKind = Nat -> Nat -> (Type -> Type) -> Type -> Type

type family Mutable (mat :: MatrixKind) = (mmat :: MMatrixKind) | mmat -> mat

class (MMatrix (Mutable mat) (G.Mutable v) a, G.Vector v a) => Matrix (mat :: MatrixKind) v a where
    dim :: mat r c v a -> (Int, Int)

    unsafeIndex :: mat r c v a -> (Int, Int) -> a

    unsafeFromVector :: (SingI r, SingI c) => v a -> mat r c v a

    -- | Convert matrix to vector in column order.
    -- Default algorithm is O((m*n) * O(unsafeIndex)).
    flatten :: mat r c v a -> v a
    flatten mat = G.generate (r*c) $ \i -> unsafeIndex mat (swap $ i `divMod` r)
      where
        (r,c) = dim mat
    {-# INLINE flatten #-}

    -- | Extract a row. Default algorithm is O(n * O(unsafeIndex)).
    unsafeTakeRow :: mat r c v a -> Int -> v a
    unsafeTakeRow mat i = G.generate c $ \j -> unsafeIndex mat (i,j)
      where
        (_,c) = dim mat
    {-# INLINE unsafeTakeRow #-}

    -- | Extract a column. Default algorithm is O(m * O(unsafeIndex)).
    unsafeTakeColumn :: mat r c v a -> Int -> v a
    unsafeTakeColumn mat j = G.generate r $ \i -> unsafeIndex mat (i,j)
      where
        (r,_) = dim mat
    {-# INLINE unsafeTakeColumn #-}

    -- | Extract the diagonal. Default algorithm is O(min(m,n) * O(unsafeIndex)).
    takeDiag :: mat r c v a -> v a
    takeDiag mat = G.generate n $ \i -> unsafeIndex mat (i,i)
      where
        n = uncurry min . dim $ mat
    {-# INLINE takeDiag #-}

    transpose :: (SingI r, SingI c) => mat r c v a -> mat c r v a
    transpose mat = unsafeFromVector $ G.generate (r*c) $ \x ->
        unsafeIndex mat $ x `divMod` c
      where
       (r, c) = dim mat
    {-# INLINE transpose #-}

    thaw :: PrimMonad s
         => mat r c v a
         -> s ((Mutable mat) r c (G.Mutable v) (PrimState s) a)

    unsafeThaw :: PrimMonad s
               => mat r c v a
               -> s ((Mutable mat) r c (G.Mutable v) (PrimState s) a)

    freeze :: PrimMonad s
           => (Mutable mat) r c (G.Mutable v) (PrimState s) a
           -> s (mat r c v a)

    unsafeFreeze :: PrimMonad s
                 => (Mutable mat) r c (G.Mutable v) (PrimState s) a
                 -> s (mat r c v a)

    map :: G.Vector v b => (a -> b) -> mat r c v a -> mat r c v b
    imap :: G.Vector v b => ((Int, Int) -> a -> b) -> mat r c v a -> mat r c v b
    imapM_ :: (Monad monad, Matrix mat v a)
           => ((Int, Int) -> a -> monad b) -> mat r c v a -> monad ()
    sequence :: (G.Vector v (monad a), Monad monad)
             => mat r c v (monad a) -> monad (mat r c v a)
    sequence_ :: (G.Vector v (monad a), Monad monad) => mat r c v (monad a) -> monad ()

-- | Derived methods

-- | Return the number of rows
rows :: Matrix m v a => m r c v a -> Int
rows = fst . dim
{-# INLINE rows #-}

-- | Return the number of columns
cols :: Matrix m v a => m r c v a -> Int
cols = snd . dim
{-# INLINE cols #-}

-- | Indexing
(!) :: forall m r c v a i j. (Matrix m v a, i <= r, j <= c)
    => m r c v a -> (Sing i, Sing j) -> a
(!) m (si, sj) = unsafeIndex m (i,j)
  where
    i = fromIntegral $ fromSing si
    j = fromIntegral $ fromSing sj
{-# INLINE (!) #-}

-- | Construct matrix from a vector containg columns.
fromVector :: forall m r c v a. (SingI r, SingI c, Matrix m v a)
           => v a -> m r c v a
fromVector vec | r*c /= n = error errMsg
               | otherwise = unsafeFromVector vec
  where
    errMsg = printf "fromVector: incorrect length (%d * %d != %d)" r c n
    n = G.length vec
    r = fromIntegral $ fromSing (sing :: Sing r)
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE fromVector #-}

matrix :: (SingI r, SingI c, Matrix m v a)
       => [[a]] -> m r c v a
matrix = fromList . concat . L.transpose
{-# INLINE matrix #-}

-- | Construct matrix from a list containg columns.
fromList :: (SingI r, SingI c, Matrix m v a)
         => [a] -> m r c v a
fromList = fromVector . G.fromList
{-# INLINE fromList #-}

-- | O(m*n) Create matrix from rows
fromRows :: (Matrix m v a, SingI r, SingI c) => [v a] -> m r c v a
fromRows = transpose . fromColumns
{-# INLINE fromRows #-}

-- | O(m*n) Create matrix from columns
fromColumns :: (Matrix m v a, SingI r, SingI c)
            => [v a] -> m r c v a
fromColumns = fromVector . G.concat
{-# INLINE fromColumns #-}

-- | O(m*n) Create a list by concatenating columns
toList :: Matrix m v a => m r c v a -> [a]
toList = G.toList . flatten
{-# INLINE toList #-}

empty :: Matrix m v a => m 0 0 v a
empty = unsafeFromVector G.empty
{-# INLINE empty #-}

create :: Matrix m v a
       => (forall s . ST s ((Mutable m) r c (G.Mutable v) s a)) -> m r c v a
create m = runST $ unsafeFreeze =<< m
{-# INLINE create #-}

-- | O(m*n) Convert to any type of matrix.
convertAny :: (Matrix m1 v1 a, Matrix m2 v2 a, SingI r, SingI c)
           => m1 r c v1 a -> m2 r c v2 a
convertAny = unsafeFromVector . G.convert . flatten
{-# INLINE convertAny #-}

-- | Extract a row.
takeRow :: forall m r c v a i. (i <= r, SingI i, Matrix m v a)
        => m r c v a -> Sing i -> v a
takeRow mat _ = unsafeTakeRow mat i
  where
    i = fromIntegral $ fromSing (sing :: Sing i)
{-# INLINE takeRow #-}

-- | O(m) Return the rows
toRows :: Matrix m v a => m r c v a -> [v a]
toRows mat = L.map (unsafeTakeRow mat) [0..r-1]
  where
    (r,_) = dim mat
{-# INLINE toRows #-}

-- | Extract a row.
takeColumn :: forall m r c v a j. (j <= c, SingI j, Matrix m v a)
           => m r c v a -> Sing j -> v a
takeColumn mat _ = unsafeTakeColumn mat j
  where
    j = fromIntegral $ fromSing (sing :: Sing j)
{-# INLINE takeColumn #-}

-- | O(m*n) Return the columns
toColumns :: Matrix m v a => m r c v a -> [v a]
toColumns mat = L.map (unsafeTakeColumn mat) [0..c-1]
  where
    (_,c) = dim mat
{-# INLINE toColumns #-}

mapM :: (G.Vector v (monad b), Monad monad, Matrix mat v a, Matrix mat v b)
     => (a -> monad b) -> mat r c v a -> monad (mat r c v b)
mapM f = sequence . map f
{-# INLINE mapM #-}

imapM :: (G.Vector v (monad b), Monad monad, Matrix mat v a, Matrix mat v b)
      => ((Int, Int) -> a -> monad b)
      -> mat r c v a -> monad (mat r c v b)
imapM f = sequence . imap f
{-# INLINE imapM #-}