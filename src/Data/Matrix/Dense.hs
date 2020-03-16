{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}
module Data.Matrix.Dense
    (
    -- * Immutable Matrix
      Matrix(..)

    -- * Accessors
    -- ** length information
    , C.dim
    , C.rows
    , C.cols

    -- ** Query
    , (C.!)
    , takeRow
    , takeColumn
    , C.takeDiag

    -- ** Unsafe Query
    , C.unsafeIndex
    , C.unsafeTakeRow
    , C.unsafeTakeColumn

    -- * Construction
    , C.empty
    , fromVector
    , fromList
    , fromRows
    , fromColumns
    , C.unsafeFromVector

    -- * Conversions
    , C.flatten
    , toRows
    , toColumns
    , C.toList

    -- * Different matrix types
    , convert

    , transpose
{-
    , subMatrix
    , ident
    , diag
    , diagRect
    , fromBlocks
    , isSymmetric
    , force

    , Data.Matrix.Generic.foldl

    -- * Mapping
    , Data.Matrix.Generic.map
    , imap

    -- * Monadic mapping
    , mapM
    , imapM
    , mapM_
    , imapM_
    , forM
    , forM_

    -- * Zipping
    , Data.Matrix.Generic.zipWith
    , Data.Matrix.Generic.zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , izipWith
    , izipWith3
    , izipWith4
    , izipWith5
    , izipWith6
    , Data.Matrix.Generic.zip
    , Data.Matrix.Generic.zip3
    , zip4
    , zip5
    , zip6

    -- * Monadic Zipping
    , zipWithM
    , zipWithM_

    -- * Unzipping
    , Data.Matrix.Generic.unzip
    , Data.Matrix.Generic.unzip3
    , unzip4
    , unzip5
    , unzip6

    -- * Monadic sequencing
    , Data.Matrix.Generic.sequence
    , Data.Matrix.Generic.sequence_

    , generate
    -}

    -- * Mutable matrix
    , C.thaw
    , C.unsafeThaw
    , C.freeze
    , C.unsafeFreeze
    , C.create
    ) where

import           Control.Arrow                     ((&&&), (***))
import           Control.DeepSeq                   hiding (force)
import           Control.Monad                     (foldM, foldM_, liftM)
import qualified Data.Foldable                     as F
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import           Prelude                           hiding (mapM, mapM_, zipWith, map)
import GHC.TypeLits (type (<=))
import Data.Singletons
import qualified Data.List as L
import Text.Printf (printf)

import           Data.Matrix.Dense.Mutable (MMatrix (..))
import qualified Data.Matrix.Internal.Class as C
import           GHC.Generics                      (Generic)

type instance C.Mutable Matrix = MMatrix

-- | Column-major matrix
data Matrix :: C.MatrixKind where
    Matrix :: (SingI r, SingI c) => v a -> Matrix r c v a

instance (G.Vector v a, Eq (v a)) => Eq (Matrix r c v a) where
    (==) (Matrix v1) (Matrix v2) = v1 == v2

instance (SingI r, SingI c, G.Vector v a, Num a) =>
    Num (Matrix r c v a) where
        m1 + m2 = zipWith (+) m1 m2
        m1 - m2 = zipWith (-) m1 m2
        m1 * m2 = zipWith (*) m1 m2
        negate = map negate
        abs = map abs
        signum = undefined
        fromInteger = undefined

instance (SingI r, SingI c, G.Vector v a, Fractional a) =>
    Fractional (Matrix r c v a) where
        m1 / m2 = zipWith (/) m1 m2
        recip = map recip
        fromRational = undefined

instance NFData (v a) => NFData (Matrix r c v a) where
    rnf (Matrix vec) = rnf vec

instance G.Vector v a => C.Matrix Matrix v a where
    -- | O(1) Return the size of matrix.
    dim :: forall r c. Matrix r c v a -> (Int, Int)
    dim (Matrix _) = (r,c)
      where
        r = fromIntegral $ fromSing (sing :: Sing r)
        c = fromIntegral $ fromSing (sing :: Sing c)
    {-# INLINE dim #-}

    -- | O(1) Unsafe indexing without bound check.
    unsafeIndex mat@(Matrix vec) (i,j) = vec `G.unsafeIndex` idx
      where
        idx = i + j * fst (C.dim mat)
    {-# INLINE unsafeIndex #-}

    -- | O(1) Create matrix from vector.
    unsafeFromVector = Matrix
    {-# INLINE unsafeFromVector #-}

    -- | O(1) Extract a row.
    unsafeTakeColumn mat@(Matrix vec) i = G.slice (i*r) r vec
      where
        (r, _) = C.dim mat
    {-# INLINE unsafeTakeColumn #-}

    -- | Create a vector by concatenating columns.
    flatten (Matrix vec) = vec
    {-# INLINE flatten #-}

    thaw (Matrix v) = MMatrix <$> G.thaw v
    {-# INLINE thaw #-}

    unsafeThaw (Matrix v) = MMatrix <$> G.unsafeThaw v
    {-# INLINE unsafeThaw #-}

    freeze (MMatrix v) = Matrix <$> G.freeze v
    {-# INLINE freeze #-}

    unsafeFreeze (MMatrix v) = Matrix <$> G.unsafeFreeze v
    {-# INLINE unsafeFreeze #-}

--reshape :: G.Vector v a => Matrix v a -> (Int, Int) -> Matrix v a

-- | Construct matrix from a vector containg columns.
fromVector :: forall m r c v a. (G.Vector v a, SingI r, SingI c)
           => v a -> Matrix r c v a
fromVector vec | r*c /= n = error errMsg
               | otherwise = C.unsafeFromVector vec
  where
    errMsg = printf "fromVector: incorrect length (%d * %d != %d)" r c n
    n = G.length vec
    r = fromIntegral $ fromSing (sing :: Sing r)
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE fromVector #-}

-- | Construct matrix from a list containg columns.
fromList :: (G.Vector v a, SingI r, SingI c)
         => [a] -> Matrix r c v a
fromList = fromVector . G.fromList
{-# INLINE fromList #-}

-- | O(m*n) Create matrix from rows
fromRows :: (G.Vector v a, SingI r, SingI c) => [v a] -> Matrix r c v a
fromRows = transpose . fromVector . G.concat
{-# INLINE fromRows #-}

-- | Extract a row.
takeRow :: forall r c v a i. (G.Vector v a, i <= r, SingI i)
        => Matrix r c v a -> Sing i -> v a
takeRow mat _ = C.unsafeTakeRow mat i
  where
    i = fromIntegral $ fromSing (sing :: Sing i)
{-# INLINE takeRow #-}

-- | O(m) Return the rows
toRows :: G.Vector v a => Matrix r c v a -> [v a]
toRows mat = L.map (C.unsafeTakeRow mat) [0..r-1]
  where
    (r,_) = C.dim mat
{-# INLINE toRows #-}

-- | Extract a row.
takeColumn :: forall r c v a j. (G.Vector v a, j <= c, SingI j)
           => Matrix r c v a -> Sing j -> v a
takeColumn mat _ = C.unsafeTakeColumn mat j
  where
    j = fromIntegral $ fromSing (sing :: Sing j)
{-# INLINE takeColumn #-}

-- | O(m*n) Return the columns
toColumns :: G.Vector v a => Matrix r c v a -> [v a]
toColumns mat = L.map (C.unsafeTakeColumn mat) [0..c-1]
  where
    (_,c) = C.dim mat
{-# INLINE toColumns #-}

-- | O(m*n) Create matrix from columns
fromColumns :: (G.Vector v a, SingI r, SingI c)
            => [v a] -> Matrix r c v a
fromColumns = fromVector . G.concat
{-# INLINE fromColumns #-}

-- | O(m*n) Matrix transpose
transpose :: G.Vector v a => Matrix m n v a -> Matrix n m v a
transpose mat@(Matrix vec) = C.unsafeFromVector $ G.generate (r*c) f
  where
    (r, c) = C.dim mat
    f i = vec G.! (i `mod` c * r + i `div` c)
{-# INLINE transpose #-}

{-
-- | O(1) Extract sub matrix
subMatrix :: G.Vector v a
          => (Int, Int)  -- ^ upper left corner of the submatrix
          -> (Int, Int)  -- ^ bottom right corner of the submatrix
          -> Matrix v a -> Matrix v a
subMatrix (i,j) (i',j') (Matrix _ _ tda offset vec)
    | m' <= 0 || n' <= 0 = C.empty
    | otherwise = Matrix m' n' tda offset' vec
  where
    m' = i' - i + 1
    n' = j' - j + 1
    offset' = offset + i * tda + j
{-# INLINE subMatrix #-}


-- | O(m*n) Create an identity matrix
ident :: (Num a, G.Vector v a) => Int -> Matrix v a
ident n = diagRect 0 (n,n) $ replicate n 1
{-# INLINE ident #-}

-- | O(m*n) Create a square matrix with given diagonal, other entries default to 0
diag :: (Num a, G.Vector v a, F.Foldable t)
     => t a  -- ^ diagonal
     -> Matrix v a
diag d = diagRect 0 (n,n) d
  where n = length . F.toList $ d
{-# INLINE diag #-}

-- | O(m*n) Create a rectangular matrix with default values and given diagonal
diagRect :: (G.Vector v a, F.Foldable t)
         => a         -- ^ default value
         -> (Int, Int)
         -> t a       -- ^ diagonal
         -> Matrix v a
diagRect z0 (r,c) d = fromVector (r,c) $ G.create $ GM.replicate n z0 >>= go d c
  where
    go xs c' v = F.foldlM f 0 xs >> return v
      where
        f !i x = GM.unsafeWrite v (i*(c'+1)) x >> return (i+1)
    n = r * c
{-# INLINE diagRect #-}

fromBlocks :: G.Vector v a
           => a               -- ^ default value
           -> [[Matrix v a]]
           -> Matrix v a
fromBlocks d ms = fromVector (m,n) $ G.create $ GM.replicate (m*n) d >>= go n ms
  where
    go n' xss v = foldM_ f 0 xss >> return v
      where
        f !cr xs = do (r', _) <- foldM g (0, 0) xs
                      return $ cr + r'
          where
            g (!maxR, !cc) x = do
                let (r,c) = C.dim x
                    vec = C.flatten x
                    step i u = do
                        GM.unsafeWrite v ((cr + i `div` c) * n' + i `mod` c + cc) u
                        return (i+1)
                G.foldM'_ step (0::Int) vec
                return (max maxR r, cc + c)
    -- figure out the dimension of the new matrix
    (m, n) = (sum *** maximum) . Prelude.unzip . Prelude.map ((maximum *** sum) .
                Prelude.unzip . Prelude.map (C.rows &&& C.cols)) $ ms
{-# INLINE fromBlocks #-}

isSymmetric :: (Eq a, G.Vector v a) => Matrix v a -> Bool
isSymmetric m@(Matrix r c _ _ _) | r /= c = False
                                 | otherwise = all f [0 .. r-1]
  where
    f i = all g [i + 1 .. c-1]
      where g j = m C.! (i,j) == m C.! (j,i)
{-# INLINE isSymmetric #-}
-}

map :: (G.Vector v a, G.Vector v b)
    => (a -> b) -> Matrix r c v a -> Matrix r c v b
map f (Matrix vec) = Matrix $ G.map f vec
{-# INLINE map #-}

imap :: (G.Vector v a, G.Vector v b)
     => ((Int, Int) -> a -> b) -> Matrix r c v a -> Matrix r c v b
imap f m@(Matrix vec) = Matrix $ G.imap f' vec
  where
    f' i = f (i `divMod` C.rows m)
{-# INLINE imap #-}

foldl :: G.Vector v b => (a -> b -> a) -> a -> Matrix r c v b -> a
foldl f acc (Matrix vec) = G.foldl f acc vec
{-# INLINE foldl #-}

mapM :: (G.Vector v a, G.Vector v b, Monad m)
     => (a -> m b) -> Matrix r c v a -> m (Matrix r c v b)
mapM f (Matrix vec) = Matrix <$> G.mapM f vec
{-# INLINE mapM #-}

-- | O(m*n) Apply the monadic action to every element and its index,
-- yielding a matrix of results.
imapM :: (G.Vector v a, G.Vector v b, Monad m)
      => ((Int, Int) -> a -> m b) -> Matrix r c v a -> m (Matrix r c v b)
imapM f m@(Matrix vec) = Matrix <$> G.imapM f' vec
  where
    f' i = f (i `divMod` C.rows m)
{-# INLINE imapM #-}

mapM_ :: (G.Vector v a, Monad m) => (a -> m b) -> Matrix r c v a -> m ()
mapM_ f = G.mapM_ f . C.flatten
{-# INLINE mapM_ #-}

-- | O(m*n) Apply the monadic action to every element and its index,
-- ignoring the results.
imapM_ :: (G.Vector v a, Monad m)
       => ((Int, Int) -> a -> m b) -> Matrix r c v a -> m ()
imapM_ f m@(Matrix vec) = G.imapM_ f' vec
  where
    f' i = f (i `divMod` C.rows m)
{-# INLINE imapM_ #-}

forM :: (G.Vector v a, G.Vector v b, Monad m)
     => Matrix r c v a -> (a -> m b) -> m (Matrix r c v b)
forM = flip mapM
{-# INLINE forM #-}

forM_ :: (G.Vector v a, Monad m) => Matrix r c v a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

zipWith :: (G.Vector v a, G.Vector v b, G.Vector v c
           , SingI n, SingI m )
        => (a -> b -> c)
        -> Matrix n m v a -> Matrix n m v b -> Matrix n m v c
zipWith f m1 m2 = fromVector $ G.zipWith f (C.flatten m1) $ C.flatten m2
{-# INLINE zipWith #-}

zipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
            , SingI n, SingI m )
         => (a -> b -> c -> d)
         -> Matrix n m v a -> Matrix n m v b -> Matrix n m v c
         -> Matrix n m v d
zipWith3 f m1 m2 m3 = fromVector $
    G.zipWith3 f (C.flatten m1) (C.flatten m2) $ C.flatten m3
{-# INLINE zipWith3 #-}

zipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e
            , SingI n, SingI m )
         => (a -> b -> c -> d -> e)
         -> Matrix n m v a
         -> Matrix n m v b
         -> Matrix n m v c
         -> Matrix n m v d
         -> Matrix n m v e
zipWith4 f m1 m2 m3 m4 = fromVector $
    G.zipWith4 f (C.flatten m1) (C.flatten m2) (C.flatten m3) $ C.flatten m4
{-# INLINE zipWith4 #-}

zipWith5 :: ( G.Vector v a, G.Vector v b, G.Vector v c,G.Vector v d
            , G.Vector v e, G.Vector v f
            , SingI n, SingI m )
         => (a -> b -> c -> d -> e -> f)
         -> Matrix n m v a
         -> Matrix n m v b
         -> Matrix n m v c
         -> Matrix n m v d
         -> Matrix n m v e
         -> Matrix n m v f
zipWith5 f m1 m2 m3 m4 m5 = fromVector $
    G.zipWith5 f (C.flatten m1) (C.flatten m2)
    (C.flatten m3) (C.flatten m4) $ C.flatten m5
{-# INLINE zipWith5 #-}

zipWith6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
            , G.Vector v e, G.Vector v f, G.Vector v g
            , SingI n, SingI m )
         => (a -> b -> c -> d -> e -> f -> g)
         -> Matrix n m v a
         -> Matrix n m v b
         -> Matrix n m v c
         -> Matrix n m v d
         -> Matrix n m v e
         -> Matrix n m v f
         -> Matrix n m v g
zipWith6 f m1 m2 m3 m4 m5 m6 = C.unsafeFromVector $
    G.zipWith6 f (C.flatten m1) (C.flatten m2) (C.flatten m3)
    (C.flatten m4) (C.flatten m5) $ C.flatten m6
{-# INLINE zipWith6 #-}

{-
izipWith :: (G.Vector v a, G.Vector v b, G.Vector v c)
         => ((Int, Int) -> a -> b -> c) -> Matrix v a -> Matrix v b -> Matrix v c
izipWith f m1 m2
    | C.dim m1 /= C.dim m2 = error "izipWith: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.izipWith f' (C.flatten m1) $ C.flatten m2
  where
    c = C.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith #-}

izipWith3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d)
          => ((Int, Int) -> a -> b -> c -> d) -> Matrix v a -> Matrix v b
          -> Matrix v c -> Matrix v d
izipWith3 f m1 m2 m3
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 = error "izipWith3: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.izipWith3 f' (C.flatten m1) (C.flatten m2) $ C.flatten m3
  where
    c = C.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith3 #-}

izipWith4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e)
          => ((Int, Int) -> a -> b -> c -> d -> e) -> Matrix v a -> Matrix v b
          -> Matrix v c -> Matrix v d -> Matrix v e
izipWith4 f m1 m2 m3 m4
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 ||
      C.dim m3 /= C.dim m4 = error "izipWith4: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.izipWith4 f' (C.flatten m1) (C.flatten m2)
                  (C.flatten m3) $ C.flatten m4
  where
    c = C.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith4 #-}

izipWith5 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
             , G.Vector v e, G.Vector v f )
          => ((Int, Int) -> a -> b -> c -> d -> e -> f) -> Matrix v a
          -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e -> Matrix v f
izipWith5 f m1 m2 m3 m4 m5
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 ||
      C.dim m3 /= C.dim m4 ||
      C.dim m4 /= C.dim m5 = error "izipWith5: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.izipWith5 f' (C.flatten m1) (C.flatten m2)
                  (C.flatten m3) (C.flatten m4) $ C.flatten m5
  where
    c = C.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith5 #-}

izipWith6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
             , G.Vector v e, G.Vector v f, G.Vector v g )
          => ((Int, Int) -> a -> b -> c -> d -> e -> f -> g) -> Matrix v a
          -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e -> Matrix v f
          -> Matrix v g
izipWith6 f m1 m2 m3 m4 m5 m6
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 ||
      C.dim m3 /= C.dim m4 ||
      C.dim m4 /= C.dim m5 ||
      C.dim m5 /= C.dim m6 = error "izipWith6: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.izipWith6 f' (C.flatten m1) (C.flatten m2) (C.flatten m3)
                  (C.flatten m4) (C.flatten m5) $ C.flatten m6
  where
    c = C.cols m1
    f' i = f (i `divMod` c)
{-# INLINE izipWith6 #-}
-}


zip :: (SingI n, SingI m, G.Vector v a, G.Vector v b, G.Vector v (a,b))
    => Matrix n m v a -> Matrix n m v b -> Matrix n m v (a,b)
zip m1 m2 = C.unsafeFromVector $ G.zip (C.flatten m1) $ C.flatten m2
{-# INLINE zip #-}

zip3 :: (SingI n, SingI m, G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c))
     => Matrix n m v a
     -> Matrix n m v b
     -> Matrix n m v c
     -> Matrix n m v (a,b,c)
zip3 m1 m2 m3 = C.unsafeFromVector $
    G.zip3 (C.flatten m1) (C.flatten m2) $ C.flatten m3
{-# INLINE zip3 #-}

{-
zip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a,b,c,d))
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v (a,b,c,d)
zip4 m1 m2 m3 m4
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 ||
      C.dim m3 /= C.dim m4 = error "zip4: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.zip4 (C.flatten m1) (C.flatten m2)
                  (C.flatten m3) $ C.flatten m4
{-# INLINE zip4 #-}

zip5 :: ( G.Vector v a, G.Vector v b, G.Vector v c
        , G.Vector v d, G.Vector v e, G.Vector v (a,b,c,d,e) )
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e
     -> Matrix v (a,b,c,d,e)
zip5 m1 m2 m3 m4 m5
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 ||
      C.dim m3 /= C.dim m4 ||
      C.dim m4 /= C.dim m5 = error "zip5: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.zip5 (C.flatten m1) (C.flatten m2)
                  (C.flatten m3) (C.flatten m4) $ C.flatten m5
{-# INLINE zip5 #-}

zip6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v e
        , G.Vector v f, G.Vector v (a,b,c,d,e,f) )
     => Matrix v a -> Matrix v b -> Matrix v c -> Matrix v d -> Matrix v e
     -> Matrix v f -> Matrix v (a,b,c,d,e,f)
zip6 m1 m2 m3 m4 m5 m6
    | C.dim m1 /= C.dim m2 ||
      C.dim m2 /= C.dim m3 ||
      C.dim m3 /= C.dim m4 ||
      C.dim m4 /= C.dim m5 ||
      C.dim m5 /= C.dim m6 = error "zip6: Dimensions don't match."
    | otherwise = C.unsafeFromVector (C.dim m1) $
                  G.zip6 (C.flatten m1) (C.flatten m2) (C.flatten m3)
                  (C.flatten m4) (C.flatten m5) $ C.flatten m6
{-# INLINE zip6 #-}

zipWithM :: (Monad m, G.Vector v a, G.Vector v b, G.Vector v c)
         => (a -> b -> m c) -> Matrix v a -> Matrix v b -> m (Matrix v c)
zipWithM f m1 m2
    | C.dim m1 /= C.dim m2 = error "zipWithM: Dimensions don't match."
    | otherwise = liftM (C.unsafeFromVector $ C.dim m1) $
                  G.zipWithM f (C.flatten m1) $ C.flatten m2
{-# INLINE zipWithM #-}

zipWithM_ :: (Monad m, G.Vector v a, G.Vector v b)
          => (a -> b -> m c) -> Matrix v a -> Matrix v b -> m ()
zipWithM_ f m1 m2
    | C.dim m1 /= C.dim m2 = error "zipWithM_: Dimensions don't match."
    | otherwise = G.zipWithM_ f (C.flatten m1) $ C.flatten m2
{-# INLINE zipWithM_ #-}

unzip :: (G.Vector v a, G.Vector v b, G.Vector v (a,b))
      => Matrix v (a,b) -> (Matrix v a, Matrix v b )
unzip m = (C.unsafeFromVector d v1, C.unsafeFromVector d v2)
  where
    d = C.dim m
    (v1, v2) = G.unzip $ C.flatten m
{-# INLINE unzip #-}

unzip3 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v (a,b,c))
       => Matrix v (a,b, c) -> (Matrix v a, Matrix v b, Matrix v c)
unzip3 m = (C.unsafeFromVector d v1, C.unsafeFromVector d v2, C.unsafeFromVector d v3)
  where
    d = C.dim m
    (v1, v2, v3) = G.unzip3 $ C.flatten m
{-# INLINE unzip3 #-}

unzip4 :: (G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d, G.Vector v (a,b,c,d))
       => Matrix v (a,b,c,d) -> (Matrix v a, Matrix v b, Matrix v c, Matrix v d)
unzip4 m = ( C.unsafeFromVector d v1
           , C.unsafeFromVector d v2
           , C.unsafeFromVector d v3
           , C.unsafeFromVector d v4
           )
  where
    d = C.dim m
    (v1, v2, v3, v4) = G.unzip4 $ C.flatten m
{-# INLINE unzip4 #-}

unzip5 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
          , G.Vector v e, G.Vector v (a,b,c,d,e) )
       => Matrix v (a,b,c,d,e)
       -> (Matrix v a, Matrix v b, Matrix v c, Matrix v d, Matrix v e)
unzip5 m = ( C.unsafeFromVector d v1
           , C.unsafeFromVector d v2
           , C.unsafeFromVector d v3
           , C.unsafeFromVector d v4
           , C.unsafeFromVector d v5
           )
  where
    d = C.dim m
    (v1, v2, v3, v4, v5) = G.unzip5 $ C.flatten m
{-# INLINE unzip5 #-}

unzip6 :: ( G.Vector v a, G.Vector v b, G.Vector v c, G.Vector v d
          , G.Vector v e, G.Vector v f, G.Vector v (a,b,c,d,e,f) )
       => Matrix v (a,b,c,d,e,f)
       -> (Matrix v a, Matrix v b, Matrix v c, Matrix v d, Matrix v e, Matrix v f)
unzip6 m = ( C.unsafeFromVector d v1
           , C.unsafeFromVector d v2
           , C.unsafeFromVector d v3
           , C.unsafeFromVector d v4
           , C.unsafeFromVector d v5
           , C.unsafeFromVector d v6
           )
  where
    d = C.dim m
    (v1, v2, v3, v4, v5, v6) = G.unzip6 $ C.flatten m
{-# INLINE unzip6 #-}
-}

sequence :: (G.Vector v a, G.Vector v (m a), Monad m)
         => Matrix r c v (m a) -> m (Matrix r c v a)
sequence (Matrix vec) = Matrix <$> G.sequence vec
{-# INLINE sequence #-}

sequence_ :: (G.Vector v (m a), Monad m)
          => Matrix r c v (m a) -> m ()
sequence_ (Matrix vec) = G.sequence_ vec
{-# INLINE sequence_ #-}

generate :: forall r c v a. (G.Vector v a, SingI r, SingI c)
         => ((Int, Int) -> a) -> Matrix r c v a
generate f = fromVector . G.generate (r*c) $ \i -> f (i `divMod` r)
  where
    r = fromIntegral $ fromSing (sing :: Sing r)
    c = fromIntegral $ fromSing (sing :: Sing c)
{-# INLINE generate #-}

-- | O(m*n) Convert different matrix type
convert :: (G.Vector v a, G.Vector w a) => Matrix r c v a -> Matrix r c w a
convert (Matrix vec) = Matrix $ G.convert vec
{-# INLINE convert #-}