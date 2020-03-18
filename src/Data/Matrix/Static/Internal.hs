{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Matrix.Static.Internal
    ( c_dd_mul
    , c_ds_mul
    , c_sd_mul
    , c_inverse
    , c_cholesky
    , c_eigs
    , c_seigs
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

             
foreign import ccall "eigen_inverse"
    c_inverse :: CInt
              -> Ptr a -> CInt -> CInt
              -> Ptr a -> CInt -> CInt
              -> IO CString

foreign import ccall "eigen_cholesky"
    c_cholesky :: CInt
               -> Ptr a -> Ptr a ->  CInt -> IO CString

foreign import ccall "spectral_eigs"
    c_eigs :: CInt -> Ptr (Complex Double)
           -> Ptr (Complex Double) -> Ptr Double -> CInt -> IO CString

foreign import ccall "spectral_seigs"
    c_seigs :: CInt -> Ptr (Complex Double) -> Ptr (Complex Double)
            -> Ptr Double -> Ptr CInt -> Ptr CInt
            -> CInt -> CInt -> IO CString