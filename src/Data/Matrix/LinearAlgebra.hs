{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Data.Matrix.LinearAlgebra
    ( (%*%)
    , inverse
    , module Data.Matrix.LinearAlgebra.Types
    , module Data.Matrix.LinearAlgebra.EigenSolver
    ) where

import Data.Vector.Storable (Vector, Storable)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable.Mutable (MVector)
import System.IO.Unsafe (unsafePerformIO)
import Data.Complex (Complex)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.ST (RealWorld)
import Data.Singletons

import qualified Data.Matrix.Dense as D
import qualified Data.Matrix.Dense.Mutable as DM
import qualified Data.Matrix.Internal.Class.Mutable as CM
import qualified Data.Matrix.Internal.Class as C
import qualified Data.Matrix.Internal.Eigen as Internal

import Data.Matrix.LinearAlgebra.Types
import Data.Matrix.LinearAlgebra.EigenSolver

-- | Multiply two matrices.
(%*%) :: forall n p m a. (SingI n, SingI m, Numeric a)
      => Matrix n p a -> Matrix p m a -> Matrix n m a
(%*%) = withFun2 Internal.c_mul
infixr 8 %*%

inverse :: (SingI r, SingI c, Numeric a) => Matrix r c a -> Matrix r c a
inverse = withFun1 Internal.c_inverse
