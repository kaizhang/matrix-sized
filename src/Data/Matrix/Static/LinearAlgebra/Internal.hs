{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Data.Matrix.Static.LinearAlgebra.Internal
    ( withFun1
    , withFun2
    , withDS
    , withSD
    , withSS
    , checkResult
    , unsafeWith
    , unsafeWith'
    , unsafeWithS
    ) where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Singletons
import Foreign
import Foreign.C.Types
import Foreign.C.String

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Dense.Mutable as DM
import qualified Data.Matrix.Static.Sparse as S
import qualified Data.Matrix.Static.Generic.Mutable as CM
import qualified Data.Matrix.Static.Generic as C
import Data.Matrix.Static.LinearAlgebra.Types

withFun1 :: forall r1 c1 r2 c2 a. (SingI r2, SingI c2, Numeric a)
         => (CInt -> Ptr a -> CInt -> CInt -> Ptr a -> CInt -> CInt -> IO CString)
         -> Matrix r1 c1 a -> Matrix r2 c2 a
withFun1 f m1 = unsafePerformIO $ do
    m0 <- CM.new
    checkResult $ unsafeWith' m0 $ \vals0 rows0 cols0 ->
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
    checkResult $ unsafeWith' m0 $ \vals0 rows0 cols0 ->
        unsafeWith m1 $ \vals1 rows1 cols1 ->
            unsafeWith m2 $ \vals2 rows2 cols2 ->
                f (foreignType (undefined :: a))
                    vals0 rows0 cols0
                    vals1 rows1 cols1
                    vals2 rows2 cols2
    C.unsafeFreeze m0
{-# INLINE withFun2 #-}

withDS :: forall r1 c1 r2 c2 r3 c3 a.
            (SingI r3, SingI c3, Numeric a)
       => ( CInt
         -> Ptr a -> CInt -> CInt
         -> Ptr a -> CInt -> CInt
         -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
         -> IO CString )
       -> Matrix r1 c1 a
       -> SparseMatrix r2 c2 a
       -> Matrix r3 c3 a
withDS f m1 m2 = unsafePerformIO $ do
    m0 <- CM.new
    checkResult $ unsafeWith' m0 $ \v0 r0 c0 ->
        unsafeWith m1 $ \v1 r1 c1 ->
            unsafeWithS m2 $ \v2 inner outer r2 c2 s ->
                f (foreignType (undefined :: a))
                    v0 r0 c0
                    v1 r1 c1
                    v2 outer inner r2 c2 s
    C.unsafeFreeze m0
{-# INLINE withDS #-}

withSD :: forall r1 c1 r2 c2 r3 c3 a.
            (SingI r3, SingI c3, Numeric a)
       => ( CInt
         -> Ptr a -> CInt -> CInt
         -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
         -> Ptr a -> CInt -> CInt
         -> IO CString )
       -> SparseMatrix r2 c2 a
       -> Matrix r1 c1 a
       -> Matrix r3 c3 a
withSD f m2 m1 = unsafePerformIO $ do
    m0 <- CM.new
    checkResult $ unsafeWith' m0 $ \v0 r0 c0 ->
        unsafeWith m1 $ \v1 r1 c1 ->
            unsafeWithS m2 $ \v2 inner outer r2 c2 s ->
                f (foreignType (undefined :: a))
                    v0 r0 c0
                    v2 outer inner r2 c2 s
                    v1 r1 c1
    C.unsafeFreeze m0
{-# INLINE withSD #-}

mkSparseMatrix :: forall r c a. (Storable a, SingI r, SingI c)
    => (Ptr (Ptr a) -> Ptr (Ptr CInt) -> Ptr CInt -> IO Int)
    -> IO (SparseMatrix r c a)
mkSparseMatrix f = do
    outer' <- VSM.new $ c + 1
    (n, pv, pinner) <- VSM.unsafeWith outer' $ \pouter -> alloca $ \ppv -> alloca $ \ppi -> do
        n <- f ppv ppi pouter
        pv <- peek ppv >>= newForeignPtr finalizerFree
        pinner <- peek ppi >>= newForeignPtr finalizerFree
        return (n, pv, pinner)
    outer <- VS.unsafeFreeze outer'
    return $ S.SparseMatrix (VS.unsafeFromForeignPtr0 pv n)
        (VS.unsafeFromForeignPtr0 pinner n)
        outer
  where
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE mkSparseMatrix #-}

withSS :: forall r1 c1 r2 c2 r3 c3 a.
            (SingI r3, SingI c3, Numeric a)
       => ( CInt
         -> Ptr (Ptr a) -> Ptr CInt -> Ptr (Ptr CInt) -> CInt -> CInt -> Ptr CInt
         -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
         -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
         -> IO CString )
       -> SparseMatrix r1 c1 a
       -> SparseMatrix r2 c2 a
       -> SparseMatrix r3 c3 a
withSS f m1 m2 = unsafePerformIO $ mkSparseMatrix $ \v0 inner0 outer0 ->
    alloca $ \pn -> unsafeWithS m1 $ \v1 inner1 outer1 r1 c1 s1 ->
        unsafeWithS m2 $ \v2 inner2 outer2 r2 c2 s2 -> do
            checkResult $ f (foreignType (undefined :: a))
                v0 outer0 inner0 r c pn
                v1 outer1 inner1 r1 c1 s1
                v2 outer2 inner2 r2 c2 s2
            fromIntegral <$> peek pn
  where
    r = fromIntegral $ fromSing (sing :: Sing r3)
    c = fromIntegral $ fromSing (sing :: Sing c3)
{-# INLINE withSS #-}

checkResult :: IO CString -> IO ()
checkResult func = func >>= \c_str -> when (c_str /= nullPtr) $
    peekCString c_str >>= \str -> error str
{-# INLINE checkResult #-}

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
            -> (Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt -> IO b)
            -> IO b
unsafeWithS mat@(S.SparseMatrix val inner outer) f = VS.unsafeWith val $ \pval ->
    VS.unsafeWith inner $ \pinner -> VS.unsafeWith outer $ \pouter ->
        f pval pinner pouter (fromIntegral r) (fromIntegral c) (fromIntegral $ VS.length val)
  where
    (r,c) = C.dim mat
{-# INLINE unsafeWithS #-}