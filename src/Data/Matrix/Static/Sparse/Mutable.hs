{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Static.Sparse.Mutable
   ( -- * Mutable sparse matrix
     MSparseMatrix(..)
   ) where

import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as S
import           Prelude                     hiding (read, replicate)
import Foreign.C.Types
import Data.Singletons
import Prelude.Singletons ()

import qualified Data.Matrix.Static.Generic.Mutable as C

-- | Column-major mutable matrix.
data MSparseMatrix :: C.MMatrixKind where
    MSparseMatrix :: (SingI r, SingI c)
                  => (v s a)         -- ^ Values: stores the coefficient values
                                      -- of the non-zeros.
                  -> (S.Vector CInt)  -- ^ InnerIndices: stores the row
                                      -- (resp. column) indices of the non-zeros.
                  -> (S.Vector CInt)  -- ^ OuterStarts: stores for each column
                                      -- (resp. row) the index of the first
                                      -- non-zero in the previous two arrays.
                  -> (S.Vector CInt)  -- InnerNNZs: stores the number of non-zeros
                                      -- of each column (resp. row). 
                  -> MSparseMatrix r c v s a

instance GM.MVector v a => C.MMatrix MSparseMatrix v a where
    dim :: forall r c s. MSparseMatrix r c v s a -> (Int, Int)
    dim (MSparseMatrix _ _ _ _) = (r,c)
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