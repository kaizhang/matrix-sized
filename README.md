Type-safe linear algebra library
================================

- General matrix types are implemented in native Haskell.

- The dimensions of matrices are statically typed.

- Provides bindings to high performance C++ linear algebra libraries such Eigen and Spectra.

Following GHC extensions may be needed:

- ScopedTypeVariables
- RankNTypes
- TypeFamilies
- DataKinds

Example
-------

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Generic as G
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Singletons.Prelude hiding ((@@))
import Data.Singletons.TypeLits
import Data.Complex
import System.Random
import Control.Monad
import Data.Type.Equality

f :: (SingI n, (2 <= n - 2) ~ 'True)
  => Matrix n n Double -> Matrix n n (Complex Double)
f m = let (d, v) = eigS (sing :: Sing 2) m
      in v @@ S.diag d @@ G.transpose v

main :: IO ()
main = do
    n <- randomRIO (2, 6)
    vals <- replicateM (n*n) $ randomRIO (-100,100) :: IO [Double]

    withSomeSing (fromIntegral n) $ \sn@(SNat :: Sing n) ->
        let s0 = SNat :: Sing 2
            sn2 = sn %- s0
        in case testEquality (s0 %<= sn2) STrue of
            Just Refl -> do
                let mat = G.fromList vals :: Matrix n n Double
                print $ f mat
            Nothing -> error $ "Requiring Matrix size >= 4, but got: " <> show n
```