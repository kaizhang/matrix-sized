{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}
module Data.Matrix.Internal.Class
    ( Mutable
    , Matrix(..)
    , MatrixKind

    -- * Derived mothods
    , rows
    , cols
    , (!)
    , empty
    , toList
    , create
    ) where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST            (ST, runST)
import qualified Data.Vector.Generic         as G
import Data.Kind (Type)
import GHC.TypeLits (Nat, type (<=))
import Data.Singletons (SingI, Sing, fromSing, sing)

import Data.Matrix.Internal.Class.Mutable (MMatrix, MMatrixKind)

type MatrixKind = Nat -> Nat -> (Type -> Type) -> Type -> Type

type family Mutable (mat :: MatrixKind) = (mmat :: MMatrixKind) | mmat -> mat

class (MMatrix (Mutable mat) (G.Mutable v) a, G.Vector v a) => Matrix (mat :: MatrixKind) v a where
    dim :: mat r c v a -> (Int, Int)

    unsafeIndex :: mat r c v a -> (Int, Int) -> a

    unsafeFromVector :: (SingI r, SingI c) => v a -> mat r c v a

    -- | Convert matrix to vector in column order.
    -- Default algorithm is O((m*n) * O(unsafeIndex)).
    flatten :: mat r c v a -> v a
    flatten mat = G.generate (r*c) $ \i -> unsafeIndex mat (i `divMod` r)
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

    {-# MINIMAL dim, unsafeIndex, unsafeFromVector, thaw, unsafeThaw, freeze, unsafeFreeze #-}

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
