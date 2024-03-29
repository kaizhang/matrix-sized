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
module Data.Matrix.Static.Dense.Mutable
   ( -- * Mutable Matrix
     MMatrix(..)
   , C.dim
   , C.unsafeWrite
   , C.unsafeRead
   , C.new
   , C.replicate
   ) where

import qualified Data.Vector.Generic.Mutable as GM
import           Prelude                     hiding (read, replicate)
import Data.Singletons
import Prelude.Singletons ()
import Control.Monad.Primitive     (PrimMonad, PrimState)

import qualified Data.Matrix.Static.Generic.Mutable as C

-- | Column-major mutable matrix.
data MMatrix :: C.MMatrixKind where
    MMatrix :: (SingI r, SingI c) => v s a -> MMatrix r c v s a

instance GM.MVector v a => C.MMatrix MMatrix v a where
    dim :: forall r c s. MMatrix r c v s a -> (Int, Int)
    dim (MMatrix _) = (r,c)
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE dim #-}

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

    unsafeModify mat@(MMatrix v) f (i,j) = GM.unsafeModify v f idx
      where 
        (r, _) = C.dim mat
        idx = i + j * r
    {-# INLINE unsafeModify #-}

    fill (MMatrix v) x = GM.set v x
    {-# INLINE fill #-}

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

{-
takeColumn :: GM.MVector v a => MMatrix v m a -> Int -> v m a
takeColumn (MMatrix _ c tda offset vec) i = GM.slice i' c vec
  where
    i' = offset + i * tda
{-# INLINE takeColumn #-}
-}