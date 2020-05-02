{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}

module Data.Matrix.Static.IO
    ( fromMM
    , fromMM'
    , toMM
    , IOElement(..)
    ) where

import qualified Data.ByteString.Char8 as B
import Conduit
import Control.Monad (when)
import qualified Data.Vector.Generic as G
import Data.Matrix.Dynamic (Dynamic(..))
import qualified Data.Vector.Unboxed as U
import           Data.ByteString.Lex.Fractional (readExponential, readSigned)
import           Data.ByteString.Lex.Integral   (readDecimal, readDecimal_)
import Data.Singletons
import Data.Maybe
import Data.Singletons.TypeLits
import Data.Double.Conversion.ByteString (toShortest)
import Text.Printf (printf)

import qualified Data.Matrix.Static.Sparse as S

data MMElem = MMReal
            | MMComplex
            | MMInteger
            | MMPattern
            deriving (Eq)

class U.Unbox a => IOElement a where
    decodeElem :: B.ByteString -> a
    encodeElem :: a -> B.ByteString
    elemType :: Proxy a -> MMElem

instance IOElement Int where
    decodeElem x = fst . fromMaybe errMsg . readSigned readDecimal $ x
      where
        errMsg = error $ "readInt: Fail to cast ByteString to Int:" ++ show x
    encodeElem = B.pack . show
    elemType _ = MMInteger

instance IOElement Double where
    decodeElem x = fst . fromMaybe errMsg . readSigned readExponential $ x
      where
        errMsg = error $ "readDouble: Fail to cast ByteString to Double:" ++ show x
    encodeElem = toShortest
    elemType _ = MMReal

fromMM' :: forall o m v a. (PrimMonad m, G.Vector v a, IOElement a)
        => ConduitT B.ByteString o m (Dynamic S.SparseMatrix v a)
fromMM' = linesUnboundedAsciiC .| do
    (ty, (r,c,nnz)) <- parseHeader
    when (elemType (Proxy :: Proxy a) /= ty) $ error "Element types do not match"
    vec <- streamTriplet .| sinkVector 
    when (U.length vec /= nnz) $ error $
        "number of non-zeros do not match: " <> show nnz <> "/=" <> show (U.length vec)
    withSomeSing (fromIntegral (r :: Int)) $ \(SNat :: Sing r) ->
        withSomeSing (fromIntegral (c :: Int)) $ \(SNat :: Sing c) ->
            return $ Dynamic (S.fromTriplet vec :: S.SparseMatrix r c v a)

fromMM :: forall o m r c v a. (PrimMonad m, SingI r, SingI c, G.Vector v a, IOElement a)
       => ConduitT B.ByteString o m (S.SparseMatrix r c v a)
fromMM = linesUnboundedAsciiC .| do
    (ty, (r,c,nnz)) <- parseHeader
    mat@(S.SparseMatrix v _ _) <- case () of
        _ | elemType (Proxy :: Proxy a) /= ty -> error "Element types do not match"
          | (r, c) /= (nrow, ncol) -> error $ "Dimensions do not match: " <>
                show (r,c) <> "/=" <> show (nrow,ncol)
          | otherwise -> do
              vec <- streamTriplet .| sinkVector 
              return $ S.fromTriplet (vec :: U.Vector (Int, Int, a))
    let n = G.length v
    if n /= nnz
        then error $ "number of non-zeros do not match: " <> show nnz <> "/=" <> show n
        else return mat
  where
    nrow = fromIntegral $ fromSing (sing :: Sing r) :: Int
    ncol = fromIntegral $ fromSing (sing :: Sing c) :: Int

toMM :: forall m r c v a i. (Monad m, S.Zero a, IOElement a, G.Vector v a)
     => S.SparseMatrix r c v a -> ConduitT i B.ByteString m ()
toMM mat@(S.SparseMatrix vec _ _) = ( do
    yield header
    yield "%"
    yield $ B.pack $ printf "%d %d %d" r c n
    S.toTriplet mat .| mapC f ) .| unlinesAsciiC
  where
    f (i, j, x) = B.unwords [B.pack $ show (i+1), B.pack $ show (j+1), encodeElem x]
    header = case elemType (Proxy :: Proxy a) of
        MMReal -> "%%MatrixMarket matrix coordinate real general"
        MMInteger -> "%%MatrixMarket matrix coordinate integer general"
        _ -> undefined
    (r, c) = S.dim mat
    n = G.length vec

parseHeader :: Monad m => ConduitT B.ByteString o m (MMElem, (Int, Int, Int))
parseHeader = do
    ty <- headC >>= \case
        Nothing -> error "Empty file"
        Just header -> return $ parse header
    dropWhileC $ (=='%') . B.head 
    headC >>= \case
        Nothing -> error "Empty file"
        Just x ->
            let [r, c, nnz] = map decodeElem $ B.words x
            in return (ty, (r, c, nnz))
  where
    parse x
        | "%%MatrixMarket" `B.isPrefixOf` x = case B.words x of
            [_, _, format, ty, form] -> 
                let ty' = case ty of
                        "real" -> MMReal
                        "complex" -> MMComplex
                        "integer" -> MMInteger
                        "pattern" -> MMPattern
                        t -> error $ "Unknown type: " <> show t
                in ty'
            _ -> error $ "Cannot parse header: " <> show x
        | otherwise = error $ "Cannot parse header: " <> show x
{-# INLINE parseHeader #-}

streamTriplet :: (Monad m, IOElement a) => ConduitT B.ByteString (Int, Int, a) m ()
streamTriplet = mapC (f . B.words)
  where
    f [i,j,x] = (readDecimal_ i - 1, readDecimal_ j - 1, decodeElem x)
    f x = error $ "Formatting error: " <> show x
{-# INLINE streamTriplet #-}
