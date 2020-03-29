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
    ( module Data.Matrix.Static.LinearAlgebra.Types
    , Arithmetic(..)
    , Factorization(..)
    , svd
    ) where

import qualified Data.Vector.Storable as VS
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
    -- | Matrix multiplication
    (@@) :: ( Numeric a, SingI n, SingI m
            , If (mat1 == mat2) mat1 D.Matrix ~ mat3 )
         => mat1 n p VS.Vector a
         -> mat2 p m VS.Vector a
         -> mat3 n m VS.Vector a
    infixr 8 @@

    (%+%) :: ( Numeric a, SingI n, SingI m
             , If (mat1 == mat2) mat1 D.Matrix ~ mat3 )
          => mat1 n m VS.Vector a
          -> mat2 n m VS.Vector a
          -> mat3 n m VS.Vector a
    infixr 8 %+%

    (%-%) :: ( Numeric a, SingI n, SingI m
             , If (mat1 == mat2) mat1 D.Matrix ~ mat3 )
          => mat1 n m VS.Vector a
          -> mat2 n m VS.Vector a
          -> mat3 n m VS.Vector a
    infixr 8 %-%

    (%*%) :: ( Numeric a, SingI n, SingI m
             , If (mat1 == mat2) mat1 S.SparseMatrix ~ mat3 )
          => mat1 n m VS.Vector a
          -> mat2 n m VS.Vector a
          -> mat3 n m VS.Vector a
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


class Factorization mat where
    -- | Matrix inverse
    inverse :: (SingI n, Numeric a) => mat n n VS.Vector a -> mat n n VS.Vector a

    -- | Eigenvalues (not ordered) and
    -- eigenvectors (as columns) of a general square matrix.
    eigs :: (SingI k, SingI n, (k <= n - 2) ~ 'True)
         => Sing k
         -> mat n n VS.Vector Double
         -> (Matrix k 1 (Complex Double), Matrix n k (Complex Double))

    -- | Cholesky decomposition
    cholesky :: (Numeric a, SingI n) => mat n n VS.Vector a -> mat n n VS.Vector a


instance Factorization D.Matrix where

    inverse = withFun1 Internal.c_inverse

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
    inverse = undefined

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

-- | Singular value decomposition
svd :: forall n p a m. (Numeric a, SingI n, SingI p, SingI m, m ~ Min n p)
    => Matrix n p a
    -> (Matrix n m a, Matrix m 1 a, Matrix p m a)
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

