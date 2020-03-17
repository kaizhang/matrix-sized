{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Data.Matrix.LinearAlgebra.Types
    ( Numeric(..)
    , Matrix
    , MMatrix
    , SparseMatrix
    , withFun1
    , withFun2
    , unsafeWith
    , unsafeWith'
    , unsafeWithS
    ) where

import Data.Vector.Storable (Vector, Storable)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable.Mutable (MVector)
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (Complex)
import Control.Monad.ST (RealWorld)
import Data.Singletons
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Int

import qualified Data.Matrix.Dense as D
import qualified Data.Matrix.Dense.Mutable as DM
import qualified Data.Matrix.Sparse as S
import qualified Data.Matrix.Internal.Class.Mutable as CM
import qualified Data.Matrix.Internal.Class as C
import qualified Data.Matrix.Internal.LinearAlgebra as Internal

class Storable a => Numeric a where
    foreignType :: a -> CInt

instance Numeric Float where foreignType _ = 0
instance Numeric Double where foreignType _ =1
instance Numeric (Complex Float) where foreignType _ = 2
instance Numeric (Complex Double) where foreignType _ = 3

type Matrix r c a = D.Matrix r c Vector a
type MMatrix r c s a = DM.MMatrix r c MVector s a

type SparseMatrix r c a = S.SparseMatrix r c Vector a

withFun1 :: forall r1 c1 r2 c2 a. (SingI r2, SingI c2, Numeric a)
         => (CInt -> Ptr a -> CInt -> CInt -> Ptr a -> CInt -> CInt -> IO CString)
         -> Matrix r1 c1 a -> Matrix r2 c2 a
withFun1 f m1 = unsafePerformIO $ do
    m0 <- CM.new
    _ <- unsafeWith' m0 $ \vals0 rows0 cols0 ->
        unsafeWith m1 $ \vals1 rows1 cols1 -> f (foreignType (undefined :: a))
            vals0 rows0 cols0
            vals1 rows1 cols1
    C.unsafeFreeze m0
{-# INLINE withFun1 #-}

withFun2 :: forall r1 c1 r2 c2 r3 c3 a.
            (SingI r3, SingI c3, Numeric a)
         => ( CInt -> Ptr a -> CInt -> CInt -> Ptr a -> CInt -> CInt
           -> Ptr a -> CInt -> CInt -> IO CString )
         -> Matrix r1 c1 a
         -> Matrix r2 c2 a
         -> Matrix r3 c3 a
withFun2 f m1 m2 = unsafePerformIO $ do
    m0 <- CM.new
    _ <- unsafeWith' m0 $ \vals0 rows0 cols0 ->
        unsafeWith m1 $ \vals1 rows1 cols1 ->
            unsafeWith m2 $ \vals2 rows2 cols2 ->
                f (foreignType (undefined :: a))
                    vals0 rows0 cols0
                    vals1 rows1 cols1
                    vals2 rows2 cols2
    C.unsafeFreeze m0
{-# INLINE withFun2 #-}

-------------------------------------------------------------------------------
-- Raw pointers
-------------------------------------------------------------------------------

-- | Pass a pointer to the matrix's data to the IO action.
-- The data may not be modified through the pointer.
unsafeWith :: Storable a => Matrix n m a -> (Ptr a -> CInt -> CInt -> IO b) -> IO b
unsafeWith mat@(D.Matrix vec) f = VS.unsafeWith vec $ \p ->
    f p (fromIntegral r) $ fromIntegral c 
  where
    (r,c) = C.dim mat
{-# INLINE unsafeWith #-}

unsafeWith' :: Storable a => MMatrix n m RealWorld a -> (Ptr a -> CInt -> CInt -> IO b) -> IO b
unsafeWith' mat@(DM.MMatrix vec) f = VSM.unsafeWith vec $ \p ->
    f p (fromIntegral r) $ fromIntegral c
  where
    (r,c) = CM.dim mat
{-# INLINE unsafeWith' #-}

-- | Pass a pointer to the matrix's data to the IO action.
-- The data may not be modified through the pointer.
unsafeWithS :: (Storable a, S.Zero a)
            => SparseMatrix n m a
            -> (Ptr a -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt -> IO b)
            -> IO b
unsafeWithS mat@(S.SparseMatrix val inner outer) f = VS.unsafeWith val $ \pval ->
    VS.unsafeWith inner $ \pinner ->
        VS.unsafeWith outer $ \pouter ->
            VS.unsafeWith innernnz $ \pnnz ->
                f pval pinner pouter pnnz (fromIntegral r) (fromIntegral c) (fromIntegral $ VS.length val)
  where
    (r,c) = C.dim mat
    innernnz = VS.generate c $ \i -> outer `VS.unsafeIndex` (i+1) - outer `VS.unsafeIndex` i
{-# INLINE unsafeWithS #-}