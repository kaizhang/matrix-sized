{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Matrix.Static.Internal
    ( c_dd_mul
    , c_ds_mul
    , c_sd_mul
    , c_ss_mul
    , c_ss_cmul
    , c_sd_plus
    , c_ss_plus
    , c_inverse
    , c_cholesky
    , c_eig
    , c_eigs
    , c_eigsh
    , c_seigs
    , c_seigsh
    , c_geigsh
    , c_bdcsvd

    , computationInfo
    ) where

import Data.Complex (Complex)
import Foreign
import Foreign.C.Types
import Foreign.C.String

-------------------------------------------------------------------------------
-- Arithmetic
-------------------------------------------------------------------------------
foreign import ccall "eigen_dd_mul"
    c_dd_mul :: CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> IO CString

foreign import ccall "eigen_ds_mul"
    c_ds_mul :: CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> IO CString

foreign import ccall "eigen_sd_mul"
    c_sd_mul :: CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> IO CString

foreign import ccall "eigen_sd_plus"
    c_sd_plus :: CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> IO CString

foreign import ccall "eigen_ss_mul"
    c_ss_mul :: CInt
          -> Ptr (Ptr a) -> Ptr CInt -> Ptr (Ptr CInt) -> CInt -> CInt -> Ptr CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> IO CString

foreign import ccall "eigen_ss_cmul"
    c_ss_cmul :: CInt
          -> Ptr (Ptr a) -> Ptr CInt -> Ptr (Ptr CInt) -> CInt -> CInt -> Ptr CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> IO CString

foreign import ccall "eigen_ss_plus"
    c_ss_plus :: CInt
          -> Ptr (Ptr a) -> Ptr CInt -> Ptr (Ptr CInt) -> CInt -> CInt -> Ptr CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> Ptr a -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt
          -> IO CString
             
foreign import ccall "eigen_inverse"
    c_inverse :: CInt
              -> Ptr a -> CInt -> CInt
              -> Ptr a -> CInt -> CInt
              -> IO CString

foreign import ccall "eigen_cholesky"
    c_cholesky :: CInt
               -> Ptr a -> Ptr a ->  CInt -> IO CString

foreign import ccall "eigen_eig"
    c_eig :: Ptr (Complex Double) -> Ptr (Complex Double)
          -> Ptr Double -> CInt -> IO CString

foreign import ccall "spectral_eigs"
    c_eigs :: CInt      -- ^ The number of eigenvectors to search
           -> Ptr (Complex Double)   -- ^ 
           -> Ptr (Complex Double)   -- ^ 
           -> Ptr Double   -- ^ Input matrix
           -> CInt         -- ^ Matrix size
           -> CInt   -- ^ ncv
           -> CInt   -- ^ max iterations
           -> Double  -- ^ tolerance
           -> CInt
           -> Double
           -> CInt
           -> IO CInt

foreign import ccall "spectral_eigsh"
    c_eigsh :: CInt
            -> Ptr Double -> Ptr Double
            -> Ptr Double -> CInt
            -> CInt -> CInt -> Double -> CInt -> Double -> CInt
            -> IO CInt

foreign import ccall "spectral_seigs"
    c_seigs :: CInt
            -> Ptr (Complex Double) -> Ptr (Complex Double)
            -> Ptr Double -> Ptr CInt -> Ptr CInt -> CInt -> CInt
            -> CInt -> CInt -> Double -> CInt -> Double -> CInt
            -> IO CInt

foreign import ccall "spectral_seigsh"
    c_seigsh :: CInt
             -> Ptr Double -> Ptr Double
             -> Ptr Double -> Ptr CInt -> Ptr CInt -> CInt -> CInt
             -> CInt -> CInt -> Double -> CInt -> Double -> CInt
             -> IO CInt

foreign import ccall "spectral_geigsh"
    c_geigsh :: CInt
             -> Ptr Double -> Ptr Double
             -> Ptr Double -> CInt
             -> Ptr Double -> Ptr CInt -> Ptr CInt -> CInt
             -> CInt -> CInt -> Double -> CInt
             -> IO CInt

foreign import ccall "eigen_bdcsvd"
    c_bdcsvd :: CInt -> Ptr a -> Ptr b -> Ptr a
             -> Ptr a -> CInt -> CInt -> IO CString

computationInfo :: CInt -> Maybe String
computationInfo 0 = Nothing
computationInfo 1 = Just "NOT_COMPUTED"
computationInfo 2 = Just "NOT_CONVERGING"
computationInfo 3 = Just "NUMERICAL_ISSUE"
computationInfo _ = Just "UNKNOWN ERROR"
{-# INLINE computationInfo #-}