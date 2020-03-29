{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Data.Matrix.Static.LinearAlgebra.Types
    ( Numeric(..)
    , Matrix
    , MMatrix
    , SparseMatrix
    ) where

import Data.Vector.Storable (Vector, Storable)
import Data.Vector.Storable.Mutable (MVector)
import Data.Complex (Complex)
import Foreign.C.Types

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Dense.Mutable as DM
import qualified Data.Matrix.Static.Sparse as S

class (S.Zero a, Storable a, Num a) => Numeric a where
    foreignType :: a -> CInt

instance Numeric Float where foreignType _ = 0
instance Numeric CFloat where foreignType _ = 0
instance Numeric Double where foreignType _ =1
instance Numeric (Complex Float) where foreignType _ = 2
instance Numeric (Complex Double) where foreignType _ = 3

type Matrix r c a = D.Matrix r c Vector a
type MMatrix r c s a = DM.MMatrix r c MVector s a

type SparseMatrix r c a = S.SparseMatrix r c Vector a