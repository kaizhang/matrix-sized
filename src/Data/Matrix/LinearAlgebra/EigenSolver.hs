{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Data.Matrix.LinearAlgebra.EigenSolver
    ( EigenDecomposition(..)
    ) where

import Data.Vector.Storable (Vector, Storable)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable.Mutable (MVector)
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (Complex)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.ST (RealWorld)
import Data.Singletons
import GHC.TypeLits (Nat, type (<=), type (-))

import Data.Matrix.LinearAlgebra.Types
import qualified Data.Matrix.Dense as D
import qualified Data.Matrix.Dense.Mutable as DM
import qualified Data.Matrix.Sparse as S
import qualified Data.Matrix.Internal.Class.Mutable as CM
import qualified Data.Matrix.Internal.Class as C
import qualified Data.Matrix.Internal.LinearAlgebra as Internal

class EigenDecomposition mat where
    -- | Eigenvalues (not ordered) and
    -- eigenvectors (as columns) of a general square matrix.
    eigs :: (SingI k, SingI n, k <= n - 2)
         => Sing k
         -> mat n n VS.Vector Double
         -> (Matrix k 1 (Complex Double), Matrix n k (Complex Double))

instance EigenDecomposition D.Matrix where
    eigs s mat = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ -> do
            unsafeWith mat $ \v n _ -> Internal.c_eigs k v1 v2 v n
        m1' <- C.unsafeFreeze m1
        m2' <- C.unsafeFreeze m2
        return (m1', m2')
      where
        k = fromIntegral $ fromSing s
    {-# INLINE eigs #-}

instance EigenDecomposition S.SparseMatrix where
    eigs s mat = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ ->
            unsafeWithS mat $ \pv pi po pnnz n _ s ->
                Internal.c_seigs k v1 v2 pv po pi pnnz n s
        m1' <- C.unsafeFreeze m1
        m2' <- C.unsafeFreeze m2
        return (m1', m2')
      where
        k = fromIntegral $ fromSing s
    {-# INLINE eigs #-}