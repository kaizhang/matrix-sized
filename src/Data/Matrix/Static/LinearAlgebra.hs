{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Matrix.Static.LinearAlgebra
    ( module Data.Matrix.Static.LinearAlgebra.Types
    , Arithmetic(..)
    , Factorization(..)
    , LinearAlgebra(..)

      -- * Dense matrix operation
    , inverse
    , eig
    , svd
    , cond
    ) where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (Complex)
import Data.Singletons.Prelude hiding ((@@), type (==))
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import qualified Data.Matrix.Static.Generic.Mutable as CM
import qualified Data.Matrix.Static.Generic as C
import qualified Data.Matrix.Static.Internal as Internal
import Data.Matrix.Static.LinearAlgebra.Types
import Data.Matrix.Static.LinearAlgebra.Internal

class Arithmetic (mat1 :: C.MatrixKind) (mat2 :: C.MatrixKind) where
    -- | Matrix multiplication between different types of matrices.
    (@@) :: ( Numeric a, SingI n, SingI m
            , If (mat1 == mat2) mat1 D.Matrix ~ mat3 )
         => mat1 n p Vector a
         -> mat2 p m Vector a
         -> mat3 n m Vector a
    infixr 8 @@

    -- | Element-wise addition between different types of matrices.
    (%+%) :: ( Numeric a, SingI n, SingI m
             , If (mat1 == mat2) mat1 D.Matrix ~ mat3 )
          => mat1 n m Vector a
          -> mat2 n m Vector a
          -> mat3 n m Vector a
    infixr 8 %+%

    -- | Element-wise substraction between different types of matrices.
    (%-%) :: ( Numeric a, SingI n, SingI m
             , If (mat1 == mat2) mat1 D.Matrix ~ mat3 )
          => mat1 n m Vector a
          -> mat2 n m Vector a
          -> mat3 n m Vector a
    infixr 8 %-%

    -- | Element-wise multiplication between different types of matrices.
    (%*%) :: ( Numeric a, SingI n, SingI m
             , If (mat1 == mat2) mat1 S.SparseMatrix ~ mat3 )
          => mat1 n m Vector a
          -> mat2 n m Vector a
          -> mat3 n m Vector a
    infixr 8 %*%

instance Arithmetic D.Matrix D.Matrix where
    (@@) = withFun2 Internal.c_dd_mul
    (%+%) = (+)
    (%-%) = (-)
    (%*%) = (*)

instance Arithmetic D.Matrix S.SparseMatrix where
    (@@) = withDS Internal.c_ds_mul
    (%+%) = flip (%+%)
    (%-%) a b = a %+% C.map negate b
    (%*%) = undefined

instance Arithmetic S.SparseMatrix D.Matrix where
    (@@) = withSD Internal.c_sd_mul
    (%+%) = withSD Internal.c_sd_plus
    (%-%) a b = a %+% C.map negate b
    (%*%) = undefined

instance Arithmetic S.SparseMatrix S.SparseMatrix where
    (@@) = withSS Internal.c_ss_mul
    (%+%) = withSS Internal.c_ss_plus
    (%-%) a b = a %+% C.map negate b
    (%*%) = withSS Internal.c_ss_cmul

class LinearAlgebra (mat :: C.MatrixKind) where
    ident :: (Numeric a, SingI n) => mat n n Vector a

instance LinearAlgebra D.Matrix where
    ident = D.diag 0 $ D.replicate 1

instance LinearAlgebra S.SparseMatrix where
    ident = S.diag $ D.replicate 1

class Factorization mat where
    -- | Eigenvalues (from largest to smallest) and
    -- eigenvectors (as columns) of a general square matrix.
    eigS :: (SingI k, SingI n, (k <= n - 2) ~ 'True)
         => Sing k
         -> mat n n Vector Double
         -> (Matrix k 1 (Complex Double), Matrix n k (Complex Double))

    -- | Eigenvalues (from largest to smallest) and
    -- eigenvectors (as columns) of a symmetric square matrix.
    eigSH :: (SingI k, SingI n, (k <= n - 1) ~ 'True)
          => Sing k
          -> mat n n Vector Double
          -> (Matrix k 1 Double, Matrix n k Double)

    -- | Cholesky decomposition
    cholesky :: (Numeric a, SingI n) => mat n n Vector a -> mat n n Vector a


instance Factorization D.Matrix where
    eigS s mat
        | D.all (==0) mat = ( D.replicate 0, D.replicate 1)
        | otherwise = unsafePerformIO $ do
            m1 <- CM.new
            m2 <- CM.new
            _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ ->
                unsafeWith mat $ \v n _ -> Internal.c_eigs k v1 v2 v n
            m1' <- C.unsafeFreeze m1
            m2' <- C.unsafeFreeze m2
            return (m1', m2')
      where
        k = fromIntegral $ fromSing s
    {-# INLINE eigS #-}

    eigSH s mat
        | D.all (==0) mat = (D.replicate 0, D.replicate 1)
        | otherwise = unsafePerformIO $ do
            m1 <- CM.new
            m2 <- CM.new
            _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ ->
                unsafeWith mat $ \v n _ -> Internal.c_eigsh k v1 v2 v n
            m1' <- C.unsafeFreeze m1
            m2' <- C.unsafeFreeze m2
            return (m1', m2')
      where
        k = fromIntegral $ fromSing s
    {-# INLINE eigSH #-}

    cholesky mat = flip withFun1 mat $
        \code p1 c1 _ p2 _ _ -> Internal.c_cholesky code p1 p2 c1
    {-# INLINE cholesky #-}

instance Factorization S.SparseMatrix where
    eigS s mat = unsafePerformIO $ do
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
    {-# INLINE eigS #-}

    eigSH s mat = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ ->
            unsafeWithS mat $ \pv pin po n _ size ->
                Internal.c_seigsh k v1 v2 pv po pin n size
        m1' <- C.unsafeFreeze m1
        m2' <- C.unsafeFreeze m2
        return (m1', m2')
      where
        k = fromIntegral $ fromSing s
    {-# INLINE eigSH #-}

    cholesky = undefined

type family R a where
    R Float = Float
    R Double = Double
    R (Complex Double) = Double
    R (Complex Float) = Float
    
-- | The inverse of a dense matrix.
inverse :: (SingI n, Numeric a) => Matrix n n a -> Matrix n n a
inverse = withFun1 Internal.c_inverse
{-# INLINE inverse #-}

-- | Compute the full eigendecomposition for dense matrix.
eig :: forall n . SingI n
    => Matrix n n Double
    -> (Matrix n 1 (Complex Double), Matrix n n (Complex Double))
eig mat = unsafePerformIO $ do
    m1 <- CM.new
    m2 <- CM.new
    _ <- unsafeWith' m1 $ \v1 _ _ -> unsafeWith' m2 $ \v2 _ _ ->
        unsafeWith mat $ \v n _ -> Internal.c_eig v1 v2 v n
    m1' <- C.unsafeFreeze m1
    m2' <- C.unsafeFreeze m2
    return (m1', m2')
{-# INLINE eig #-}

-- | Compute the full singular value decomposition for dense matrix.
svd :: forall n p a m. (Numeric (R a), Numeric a, SingI n, SingI p, SingI m, m ~ Min n p)
    => Matrix n p a
    -> (Matrix n m a, Matrix m 1 (R a), Matrix p m a)
svd mat = unsafePerformIO $ do
    mu <- CM.new
    ms <- CM.new
    mv <- CM.new
    checkResult $ unsafeWith' mu $ \pu _ _ -> unsafeWith' ms $ \ps _ _ ->
        unsafeWith' mv $ \pv _ _ -> unsafeWith mat $ \px r c ->
            Internal.c_bdcsvd (foreignType (undefined :: a))
                pu ps pv px r c
    u <- C.unsafeFreeze mu
    s <- C.unsafeFreeze ms
    v <- C.unsafeFreeze mv
    return (u, s, v)
{-# INLINE svd #-}

-- | Condition number.
cond :: ( Numeric a, Numeric (R a), Ord (R a), Fractional (R a)
        , SingI n, SingI m, SingI (Min n m))
     => Matrix n m a -> R a
cond mat = VS.maximum val / VS.minimum val
  where
    val = VS.filter (/=0) $ D.flatten s
    (_,s,_) = svd mat
{-# INLINE cond #-}