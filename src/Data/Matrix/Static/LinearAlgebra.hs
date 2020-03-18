{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
module Data.Matrix.Static.LinearAlgebra
    ( Arithmetic(..)
    , Factorization(..)
    , inverse
    , module Data.Matrix.Static.LinearAlgebra.Types
    ) where

import qualified Data.Vector.Storable as VS
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (Complex)
import Data.Singletons
import GHC.TypeLits (type (<=), type (-))

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import qualified Data.Matrix.Static.Generic.Mutable as CM
import qualified Data.Matrix.Static.Generic as C
import qualified Data.Matrix.Static.Internal as Internal
import Data.Matrix.Static.LinearAlgebra.Types

class Arithmetic (mat1 :: C.MatrixKind)
                 (mat2 :: C.MatrixKind)
                 (mat3 :: C.MatrixKind) |
                 mat1 mat2 -> mat3 where
    (%*%) :: (Numeric a, SingI n, SingI m)
          => mat1 n p VS.Vector a
          -> mat2 p m VS.Vector a
          -> mat3 n m VS.Vector a
    infixr 8 %*%

instance Arithmetic D.Matrix D.Matrix D.Matrix where
    (%*%) = withFun2 Internal.c_dd_mul

instance Arithmetic D.Matrix S.SparseMatrix D.Matrix where
    (%*%) = withDS Internal.c_ds_mul

instance Arithmetic S.SparseMatrix D.Matrix D.Matrix where
    (%*%) = withSD Internal.c_sd_mul

instance Arithmetic S.SparseMatrix S.SparseMatrix S.SparseMatrix where
    (%*%) = undefined

inverse :: (SingI n, Numeric a) => Matrix n n a -> Matrix n n a
inverse = withFun1 Internal.c_inverse

class Factorization mat where
    -- | Eigenvalues (not ordered) and
    -- eigenvectors (as columns) of a general square matrix.
    eigs :: (SingI k, SingI n, k <= n - 2)
         => Sing k
         -> mat n n VS.Vector Double
         -> (Matrix k 1 (Complex Double), Matrix n k (Complex Double))

    -- | Cholesky decomposition
    cholesky :: (Numeric a, SingI n) => mat n n VS.Vector a -> mat n n VS.Vector a

instance Factorization D.Matrix where
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

    cholesky mat = flip withFun1 mat $
        \code p1 c1 _ p2 _ _ -> Internal.c_cholesky code p1 p2 c1
    {-# INLINE cholesky #-}

instance Factorization S.SparseMatrix where
    eigs s mat = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ ->
            unsafeWithS mat $ \pv pin po n _ size ->
                Internal.c_seigs k v1 v2 pv po pin n size
        m1' <- C.unsafeFreeze m1
        m2' <- C.unsafeFreeze m2
        return (m1', m2')
      where
        k = fromIntegral $ fromSing s
    {-# INLINE eigs #-}

    cholesky = undefined