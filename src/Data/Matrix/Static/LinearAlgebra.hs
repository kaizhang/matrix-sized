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
{-# LANGUAGE RecordWildCards #-}
module Data.Matrix.Static.LinearAlgebra
    ( module Data.Matrix.Static.LinearAlgebra.Types
    , Arithmetic(..)
    , Factorization(..)
    , LinearAlgebra(..)
    , EigenOptions(..)
    , defaultEigenOptions
    , SortRule(..)

      -- * Dense matrix operation
    , zeros
    , ones
    , inverse
    , eig
    , svd
    , cond
    ) where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (Complex)
import Data.Singletons.Prelude hiding ((@@), type (==), type (-), type (<=))
import Data.Type.Equality
import GHC.TypeLits
import Data.Maybe (fromMaybe)

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

    colSum :: (Numeric a, SingI n, C.Matrix mat Vector a)
           => mat m n Vector a
           -> Matrix 1 n a
    colSum mat = D.create $ do
        m <- CM.replicate 0
        flip C.imapM_ mat $ \(_,j) v -> CM.unsafeModify m (+v) (0, j)
        return m
    {-# INLINE colSum #-}

    rowSum :: (Numeric a, SingI m, C.Matrix mat Vector a)
           => mat m n Vector a
           -> Matrix m 1 a
    rowSum mat = D.create $ do
        m <- CM.replicate 0
        flip C.imapM_ mat $ \(i,_) x -> CM.unsafeModify m (+x) (i, 0)
        return m
    {-# INLINE rowSum #-}

instance LinearAlgebra D.Matrix where
    ident = D.diag 0 $ D.replicate 1

instance LinearAlgebra S.SparseMatrix where
    ident = S.diag $ D.replicate 1

data EigenOptions = EigenOptions
    { _sigma :: Maybe Double -- ^ Find eigenvalues near sigma using shift-invert mode.
    , _ncv :: Maybe Int      -- ^ Parameter that controls the convergence
                             -- speed of the algorithm. Typically a larger ncv
                             -- means faster convergence, but it may also result
                             -- in greater memory use and more matrix operations
                             -- in each iteration. This parameter must satisfy
                             -- k+2 <= ncv <= n, and is advised to take ncv > 2k+1.
                             -- Default: min(n, max(2*k + 1, 20))
    , _maxit :: Maybe Int          -- ^ Maximum number of iterations allowed in the algorithm.
    , _tol   :: Double       -- Precision parameter for the calculated eigenvalues.
    , _sort_rule :: SortRule
    }

defaultEigenOptions :: EigenOptions
defaultEigenOptions = EigenOptions
    { _sigma = Nothing
    , _ncv = Nothing
    , _maxit = Nothing
    , _tol = 2.2204460492503131e-16
    , _sort_rule = LM }

data SortRule = LM
              | LR
              | LI
              | LA
              | SM
              | SR
              | SI
              | SA
              | BE
              deriving (Enum)

class Factorization mat where
    -- | Eigenvalues (from largest to smallest) and
    -- eigenvectors (as columns) of a general square matrix.
    eigS :: (SingI k, SingI n, k <= n - 2)
         => Sing k
         -> mat n n Vector Double
         -> (Matrix k 1 (Complex Double), Matrix n k (Complex Double))
    eigS a b = eigS' a b defaultEigenOptions

    eigS' :: (SingI k, SingI n, k <= n - 2)
         => Sing k
         -> mat n n Vector Double
         -> EigenOptions
         -> (Matrix k 1 (Complex Double), Matrix n k (Complex Double))

    -- | Eigenvalues (from largest to smallest) and
    -- eigenvectors (as columns) of a symmetric square matrix.
    eigSH :: (SingI k, SingI n, k <= n - 1)
          => Sing k
          -> mat n n Vector Double
          -> (Matrix k 1 Double, Matrix n k Double)
    eigSH a b = eigSH' a b defaultEigenOptions

    eigSH' :: (SingI k, SingI n, k <= n - 1)
           => Sing k
           -> mat n n Vector Double
           -> EigenOptions
           -> (Matrix k 1 Double, Matrix n k Double)

    -- | Generalized eigen solver for real symmetric matrices
    geigSH :: (SingI k, SingI n, k <= n - 1)
           => Sing k
           -> mat n n Vector Double
           -> SparseMatrix n n Double
           -> (Matrix k 1 Double, Matrix n k Double)
    geigSH a b c = geigSH' a b c defaultEigenOptions

    geigSH' :: (SingI k, SingI n, k <= n - 1)
            => Sing k
            -> mat n n Vector Double
            -> SparseMatrix n n Double
            -> EigenOptions
            -> (Matrix k 1 Double, Matrix n k Double)

    -- | Cholesky decomposition
    cholesky :: (Numeric a, SingI n) => mat n n Vector a -> mat n n Vector a


instance Factorization D.Matrix where
    eigS' s mat EigenOptions{..}
        | D.all (==0) mat = ( D.replicate 0, D.replicate 1)
        | otherwise = unsafePerformIO $ do
            m1 <- CM.new
            m2 <- CM.new
            info <- fmap Internal.computationInfo $ unsafeWith' m1 $ \v1 _ _ ->
                unsafeWith' m2 $ \v2 _ _ -> unsafeWith mat $ \v n _ ->
                    Internal.c_eigs k v1 v2 v n ncv' maxit _tol mode sigma $ fromIntegral $ fromEnum _sort_rule
            case info of
                Nothing -> do
                    m1' <- C.unsafeFreeze m1
                    m2' <- C.unsafeFreeze m2
                    return (m1', m2')
                Just err -> error err
      where
        ncv' = case _ncv of
            Just x -> fromIntegral x
            Nothing -> min d $ max 20 $ 2 * k + 1
        maxit = case _maxit of
            Just x -> fromIntegral x
            Nothing -> d * 10
        mode = case _sigma of
            Nothing -> 0
            _ -> 1
        sigma = fromMaybe 0 _sigma
        k = fromIntegral $ fromSing s
        d = fromIntegral $ D.rows mat
    {-# INLINE eigS' #-}

    eigSH' s mat EigenOptions{..}
        | D.all (==0) mat = (D.replicate 0, D.replicate 1)
        | otherwise = unsafePerformIO $ do
            m1 <- CM.new
            m2 <- CM.new
            ( fmap Internal.computationInfo $ unsafeWith' m1 $ \v1 _ _ ->
                unsafeWith' m2 $ \v2 _ _ -> unsafeWith mat $ \v n _ ->
                    Internal.c_eigsh k v1 v2 v n ncv' maxit _tol mode sigma $
                    fromIntegral $ fromEnum _sort_rule ) >>= maybe
                    ( do
                        m1' <- C.unsafeFreeze m1
                        m2' <- C.unsafeFreeze m2
                        return (m1', m2') ) error
      where
        ncv' = case _ncv of
            Just x -> fromIntegral x
            Nothing -> min d $ max 20 $ 2 * k + 1
        maxit = case _maxit of
            Just x -> fromIntegral x
            Nothing -> d * 10
        mode = case _sigma of
            Nothing -> 0
            _ -> 1
        sigma = fromMaybe 0 _sigma
        k = fromIntegral $ fromSing s
        d = fromIntegral $ D.rows mat
    {-# INLINE eigSH' #-}

    geigSH' s matA matB EigenOptions{..} = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        ( fmap Internal.computationInfo $ unsafeWith' m1 $ \v1 _ _ ->
            unsafeWith' m2 $ \v2 _ _ -> unsafeWith matA $ \v n _ ->
            unsafeWithS matB $ \pv pin po _ _ size ->
            Internal.c_geigsh k v1 v2 v n pv po pin size
            ncv' maxit _tol $ fromIntegral $ fromEnum _sort_rule ) >>= maybe
            ( do
                m1' <- C.unsafeFreeze m1
                m2' <- C.unsafeFreeze m2
                return (m1', m2') ) error
      where
        ncv' = case _ncv of
            Just x -> fromIntegral x
            Nothing -> min d $ max 20 $ 2 * k + 1
        maxit = case _maxit of
            Just x -> fromIntegral x
            Nothing -> d * 10
        k = fromIntegral $ fromSing s
        d = fromIntegral $ D.rows matA
    {-# INLINE geigSH' #-}

    cholesky mat = flip withFun1 mat $
        \code p1 c1 _ p2 _ _ -> Internal.c_cholesky code p1 p2 c1
    {-# INLINE cholesky #-}

instance Factorization S.SparseMatrix where
    eigS' s mat EigenOptions{..} = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        ( fmap Internal.computationInfo $ unsafeWith' m1 $ \v1 _ _ ->
            unsafeWith' m2 $ \v2 _ _ -> unsafeWithS mat $ \pv pin po n _ size ->
                Internal.c_seigs k v1 v2 pv po pin n size
                ncv' maxit _tol mode sigma $ fromIntegral $ fromEnum _sort_rule
                ) >>= maybe
            ( do
                m1' <- C.unsafeFreeze m1
                m2' <- C.unsafeFreeze m2
                return (m1', m2') ) error
      where
        ncv' = case _ncv of
            Just x -> fromIntegral x
            Nothing -> min d $ max 20 $ 2 * k + 1
        maxit = case _maxit of
            Just x -> fromIntegral x
            Nothing -> d * 10
        mode = case _sigma of
            Nothing -> 0
            _ -> 1
        sigma = fromMaybe 0 _sigma
        k = fromIntegral $ fromSing s
        d = fromIntegral $ D.rows mat
    {-# INLINE eigS' #-}

    eigSH' s mat EigenOptions{..} = unsafePerformIO $ do
        m1 <- CM.new
        m2 <- CM.new
        ( fmap Internal.computationInfo $ unsafeWith' m1 $ \v1 _ _ ->
            unsafeWith' m2 $ \v2 _ _ -> unsafeWithS mat $ \pv pin po n _ size ->
                Internal.c_seigsh k v1 v2 pv po pin n size
                ncv' maxit _tol mode sigma $ fromIntegral $ fromEnum _sort_rule
                ) >>= maybe
                ( do 
                    m1' <- C.unsafeFreeze m1
                    m2' <- C.unsafeFreeze m2
                    return (m1', m2') ) error
      where
        ncv' = case _ncv of
            Just x -> fromIntegral x
            Nothing -> min d $ max 20 $ 2 * k + 1
        maxit = case _maxit of
            Just x -> fromIntegral x
            Nothing -> d * 10
        mode = case _sigma of
            Nothing -> 0
            _ -> 1
        sigma = fromMaybe 0 _sigma
        k = fromIntegral $ fromSing s
        d = fromIntegral $ D.rows mat
    {-# INLINE eigSH' #-}

    cholesky = undefined

type family R a where
    R Float = Float
    R Double = Double
    R (Complex Double) = Double
    R (Complex Float) = Float

zeros :: (SingI m, SingI n) => Matrix m n Double
zeros = D.replicate 0
{-# INLINE zeros #-}

ones :: (SingI m, SingI n) => Matrix m n Double
ones = D.replicate 1
{-# INLINE ones #-}
    
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
