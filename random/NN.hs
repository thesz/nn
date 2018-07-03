-- |NN.hs
--
-- Construct and evaluate neural networks.
--
-- Copyright (C) 2018 Serguey Zefirov.

{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE PolyKinds, RankNTypes, AllowAmbiguousTypes  #-}

module NN(module NN, module Ops, module Data.Proxy, module GHC.TypeLits) where

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map

import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import GHC.Conc (getNumCapabilities)

import GHC.TypeLits

import Ops

data Parts r = Parts !(Map.Map String r)

-- |Add two parts.
combineParts :: (r -> r -> r) -> Parts r -> Parts r -> Parts r
combineParts comb (Parts p1) (Parts p2) = Parts $ Map.unionWith comb p1 p2

-- |Combinator to remove a named part.
removePart :: String -> State (Parts r) ()
removePart n = do
	Parts m <- get
	put $ Parts $ Map.delete n m

-- |Fetch a value from parts map.
getValue :: String -> State (Parts r) r
getValue n =
	liftM (\(Parts m) -> Map.findWithDefault (error $ "unable to obtain a value "++show n++" from named parts dict") n m) get

-- |Add a value to or replace it in parts map.
setValue :: String -> r -> State (Parts r) ()
setValue n v = do
	Parts m <- get
	put $ Parts $ Map.insert n v m

-- |@randoms@ must be infinite list of infinite lists.
-- @actions@ list control when the validate current state (True as current head) and when to stop (no actions left).
-- constructor and extractor are for 
optimizeExpr :: expr Double -> trainData -> (Int -> trainData -> V.Vector trainData)
	-> (forall r . expr r -> UV.Vector r)
	-> (forall a r . UV.Vector r -> expr a -> expr r)
	-> (forall r . Floating r => expr r -> trainData -> (Parts r, Parts (UV.Vector r)))
	-> (forall r . Floating r => Parts (UV.Vector r) -> Parts r)
	-> [[Double]]
	-> [Bool]
	-> (expr Double -> IO ())
	-> IO (expr Double)
optimizeExpr expr trainData splitter paramsExtractor exprConstructor evaluator finalReducer randoms actions validate = do
	nCores <- getNumCapabilities
	let	trainDataSplit = splitter nCores trainData
	loop expr trainDataSplit actions randoms
	where
		evalToPair mv f x = do
			let	(a,b) = f x
			a `seq` b `seq` putMVar mv (a,b)
		loop expr _ [] _ = return expr
		loop expr splitTrainData (check:actions) (randoms:randomss) = do
			when check $ validate expr
			let	acts = flip map (V.toList splitTrainData) $ \tdp -> do
				mv <- newEmptyMVar
				forkIO $ evalToPair mv (evaluator expr) tdp
				return mv
			mvs <- sequence acts
			Parts scoreParts <- wait (Parts Map.empty) (Parts Map.empty) mvs
			putStrLn $ "score parts:"
			forM_ (Map.toList scoreParts) $ \(name, cba) -> do
				putStrLn $ "    " ++ name ++ ": " ++ show cba
			let	summaryScore = Map.foldr (+) 0 scoreParts
			putStrLn $ "    smuuary score: " ++ show summaryScore
			loop expr splitTrainData actions randomss
			where
				currentParams = paramsExtractor expr
				wait scalars vectors [] = return $ combineParts (+) scalars $ finalReducer vectors

