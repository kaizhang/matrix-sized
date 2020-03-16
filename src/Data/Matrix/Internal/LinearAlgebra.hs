{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Matrix.Internal.LinearAlgebra
    ( c_mul
    , c_inverse
    , c_eigs
    ) where

import Data.Complex (Complex)
import Foreign
import Foreign.C.Types

import Foreign.C.String

foreign import ccall "eigen_mul"
    c_mul :: CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> Ptr a -> CInt -> CInt
          -> IO CString
             
foreign import ccall "eigen_inverse"
    c_inverse :: CInt
              -> Ptr a -> CInt -> CInt
              -> Ptr a -> CInt -> CInt
              -> IO CString

foreign import ccall "spectral_eigs"
    c_eigs :: CInt -> Ptr (Complex Double)
           -> Ptr (Complex Double) -> Ptr Double -> CInt -> IO CString