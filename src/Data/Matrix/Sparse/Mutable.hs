{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Sparse.Mutable
   ( -- * Mutable sparse matrix
     MSparseMatrix(..)
   ) where

import           Control.DeepSeq
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as S
import           Prelude                     hiding (read, replicate)
import Data.Singletons
import Control.Monad.Primitive     (PrimMonad, PrimState)

import qualified Data.Matrix.Internal.Class.Mutable as C

-- | Column-major mutable matrix.
data MSparseMatrix :: C.MMatrixKind where
    MSparseMatrix :: (SingI r, SingI c)
                  => !(v s a)         -- ^ Values: stores the coefficient values
                                      -- of the non-zeros.
                  -> !(S.Vector Int)  -- ^ InnerIndices: stores the row
                                      -- (resp. column) indices of the non-zeros.
                  -> !(S.Vector Int)  -- ^ OuterStarts: stores for each column
                                      -- (resp. row) the index of the first
                                      -- non-zero in the previous two arrays.
                  -> MSparseMatrix r c v s a

instance (NFData (v s a)) => NFData (MSparseMatrix r c v s a) where
    rnf (MSparseMatrix vec inner outer) = rnf vec

instance GM.MVector v a => C.MMatrix MSparseMatrix v a where
    dim :: forall r c s. MSparseMatrix r c v s a -> (Int, Int)
    dim (MSparseMatrix _ _ _) = (r,c)
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE dim #-}

{-
    unsafeRead mat@(MMatrix v) (i,j) = GM.unsafeRead v idx
      where
        (r, _) = C.dim mat
        idx = i + j * r
    {-# INLINE unsafeRead #-}

    unsafeWrite mat@(MMatrix v) (i,j) = GM.unsafeWrite v idx
      where 
        (r, _) = C.dim mat
        idx = i + j * r
    {-# INLINE unsafeWrite #-}

    new :: forall r c s. (SingI r, SingI c, PrimMonad s)
        => s (MMatrix r c v (PrimState s) a)
    new = MMatrix <$> GM.new (r*c)
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE new #-}

    replicate :: forall r c s. (SingI r, SingI c, PrimMonad s)
              => a -> s (MMatrix r c v (PrimState s) a)
    replicate x = MMatrix <$> GM.replicate (r*c) x
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE replicate #-}
    -}