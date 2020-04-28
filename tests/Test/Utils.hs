{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Test.Utils where

import Test.Tasty.QuickCheck
import Data.Complex
import Data.Matrix.Static.LinearAlgebra
import Data.Singletons
import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Sparse as S
import Data.Vector.Storable (Storable)
import Data.List.Ordered
import qualified Data.Vector as V
import Data.Ord
import qualified Data.Vector.Generic as G
import Data.AEq
import Control.Monad

class Approx a where
    (~=) :: a -> a -> Bool
    infixl 4 ~=

instance Approx Double where
    a ~= b = abs (a - b) < 1e-10

instance Approx Float where
    a ~= b = abs (a - b) < 1e-3

instance Approx (Complex Double) where
    a ~= b = r1 ~= r2 && i1 ~= i2
      where
        (r1, i1) = polar a
        (r2, i2) = polar b

instance (SingI m, SingI n, Storable a, Approx a) => Approx (Matrix m n a) where
    m1 ~= m2 = D.all id $ D.zipWith (~=) m1 m2

instance (G.Vector v a, Arbitrary a, SingI m, SingI n)
    => Arbitrary (D.Matrix m n v a) where
        arbitrary = D.fromList <$> vector (m*n)
          where
            m = fromIntegral $ fromSing (sing :: Sing m)
            n = fromIntegral $ fromSing (sing :: Sing n)
        shrink _v = []

instance (G.Vector v a, Arbitrary a, SingI m, SingI n, S.Zero a)
    => Arbitrary (S.SparseMatrix m n v a) where
        arbitrary = do
            vals <- filter (/=S.zero) <$> vector p
            xs <- fmap (nubSortBy (comparing fst)) $ forM vals $ \v -> do
                i <- choose (0, m-1)
                j <- choose (0, n-1)
                return ((i,j),v)
            return $ S.fromTriplet $ V.fromList $ map (\((a,b),c) -> (a,b,c)) xs
          where
            p = (m * n) `div` 10
            m = fromIntegral $ fromSing (sing :: Sing m)
            n = fromIntegral $ fromSing (sing :: Sing n)
        shrink _v = []