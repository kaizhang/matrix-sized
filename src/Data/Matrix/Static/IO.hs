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
    , withMM
    ) where

import qualified Data.ByteString.Char8 as B
import Conduit
import Control.Monad (when)
import qualified Data.Vector.Generic as G
import           Data.ByteString.Lex.Fractional (readExponential, readSigned)
import           Data.ByteString.Lex.Integral   (readDecimal, readDecimal_)
import Data.Singletons
import Data.Maybe
import Data.Singletons.TypeLits

import qualified Data.Matrix.Static.Sparse as S

data MMElem = MMReal
            | MMComplex
            | MMInteger
            | MMPattern
            deriving (Eq)

class IOElement a where
    decodeElem :: B.ByteString -> a
    elemType :: Proxy a -> MMElem

instance IOElement Int where
    decodeElem x = fst . fromMaybe errMsg . readSigned readDecimal $ x
      where
        errMsg = error $ "readInt: Fail to cast ByteString to Int:" ++ show x
    elemType _ = MMInteger

instance IOElement Double where
    decodeElem x = fst . fromMaybe errMsg . readSigned readExponential $ x
      where
        errMsg = error $ "readDouble: Fail to cast ByteString to Double:" ++ show x
    elemType _ = MMReal

withMM :: forall m v a b. (Monad m, G.Vector v a, IOElement a)
       => ConduitT () B.ByteString m ()
       -> (forall r c. S.SparseMatrix r c v a -> m b)
       -> m b
withMM conduit f = do
    (ty, (r,c,nnz)) <- parseHeader conduit
    when (elemType (Proxy :: Proxy a) /= ty) $ error "Element types do not match"
    withSomeSing (fromIntegral (r :: Int)) $ \(SNat :: Sing r) ->
        withSomeSing (fromIntegral (c :: Int)) $ \(SNat :: Sing c) -> do
            mat@(S.SparseMatrix v _ _) <- S.fromTripletC triplets
            let n = G.length v
            when (n /= nnz) $ error $
                "number of non-zeros do not match: " <> show nnz <> "/=" <> show n
            f (mat :: S.SparseMatrix r c v a)
  where
    triplets = conduit .| linesUnboundedAsciiC .| filterC (not . (=='%') . B.head) .|
        (dropC 1 >> streamTriplet)

fromMM :: forall m r c v a. (Monad m, SingI r, SingI c, G.Vector v a, IOElement a)
       => ConduitT () B.ByteString m () -> m (S.SparseMatrix r c v a)
fromMM conduit = do
    (ty, (r,c,nnz)) <- parseHeader conduit
    mat@(S.SparseMatrix v _ _) <- case () of
        _ | elemType (Proxy :: Proxy a) /= ty -> error "Element types do not match"
          | (r, c) /= (nrow, ncol) -> error $ "Dimensions do not match: " <>
                show (r,c) <> "/=" <> show (nrow,ncol)
          | otherwise -> S.fromTripletC triplets
    let n = G.length v
    if n /= nnz
        then error $ "number of non-zeros do not match: " <> show nnz <> "/=" <> show n
        else return mat
  where
    triplets = conduit .| linesUnboundedAsciiC .| filterC (not . (=='%') . B.head) .|
        (dropC 1 >> streamTriplet)
    nrow = fromIntegral $ fromSing (sing :: Sing r) :: Int
    ncol = fromIntegral $ fromSing (sing :: Sing c) :: Int

parseHeader :: Monad m => ConduitT () B.ByteString m () -> m (MMElem, (Int, Int, Int))
parseHeader conduit = runConduit $
    conduit .| linesUnboundedAsciiC .| headerParser
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
    headerParser = do
        ty <- headC >>= \case
            Nothing -> error "Empty file"
            Just header -> return $ parse header
        line <- filterC (not . (=='%') . B.head) .| headC
        case line of
            Nothing -> error "Empty file"
            Just x ->
                let [r, c, nnz] = map decodeElem $ B.words x
                in return (ty, (r, c, nnz))
{-# INLINE parseHeader #-}

streamTriplet :: (Monad m, IOElement a) => ConduitT B.ByteString (Int, Int, a) m ()
streamTriplet = mapC (f . B.words)
  where
    f [i,j,x] = (readDecimal_ i, readDecimal_ j, decodeElem x)
    f x = error $ "Formatting error: " <> show x
{-# INLINE streamTriplet #-}