{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Matrix.Class.Mutable
    ( MMatrix(..)
    , MMatrixKind
    ) where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GM
import           Prelude                     hiding (read)
import Data.Kind (Type)
import GHC.TypeLits (Nat)
import Data.Singletons (SingI)

type MMatrixKind = Nat -> Nat -> (Type -> Type -> Type) -> Type -> Type -> Type

class GM.MVector v a => MMatrix (mat :: MMatrixKind) v a where
    dim :: mat r c v s a -> (Int, Int)

    unsafeRead :: PrimMonad s => mat r c v (PrimState s) a -> (Int, Int) -> s a

    unsafeWrite :: PrimMonad s => mat r c v (PrimState s) a -> (Int, Int) -> a -> s ()

    -- | Create a mutable matrix without initialization
    new :: (SingI r, SingI c, PrimMonad s) => s (mat r c v (PrimState s) a)

    replicate :: (SingI r, SingI c, PrimMonad s) => a -> s (mat r c v (PrimState s) a)

    {-# MINIMAL dim, unsafeRead, unsafeWrite, new, replicate #-}

{-
write :: (PrimMonad s, MMatrix m v a)
      => m v (PrimState s) a -> (Int, Int) -> a -> s ()
write mat (i,j)
    | i < 0 || i >= r || j < 0 || j >= c = error "write: Index out of bounds"
    | otherwise = unsafeWrite mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE write #-}

read :: (PrimMonad s, MMatrix m v a)
     => m v (PrimState s) a -> (Int, Int) -> s a
read mat (i,j)
    | i <0 || i >= r || j < 0 || j >= c = error "read: Index out of bounds"
    | otherwise = unsafeRead mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE read #-}
-}