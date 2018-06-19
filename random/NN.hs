-- |NN.hs
--
-- Construct and evaluate neural networks.
--
-- Copyright (C) 2018 Serguey Zefirov.

{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE PolyKinds, RankNTypes #-}

--{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver   #-}

module Eval where

import Data.Proxy

import Data.Vector.Unboxed as UV

import GHC.TypeLits

import Ops

infixl 6 .+, .-
infixl 7 .*, ./
class Floating a => NNVal a where
	(.+), (.-), (.*), (./) :: a -> a -> a
	(.+) = (+)
	(.-) = (-)
	(.*) = (*)
	(./) = (/)

instance NNVal a => NNVal (CBA a)

instance NNVal Float
instance NNVal Double

data Block ty (isize :: Nat) (osize :: Nat) where
	-- |Composition.
	Compose :: (KnownNat isize, KnownNat intsize, KnownNat osize, NNVal ty) =>
		Int -> Block ty isize intsize -> Int -> Block ty intsize osize -> Block ty isize osize
	-- |Simple transform (sigmoid, for example).
	Map :: NNVal ty => (ty -> ty) -> Block ty size size
	-- |Parametrized transform - like parametric/exponential rectifiers, etc.
	PMap :: NNVal ty => Int -> (UV.Vector ty -> ty -> ty) -> Block ty size size
	-- |Shift - addition of a vector (parameters).
	Shift :: NNVal ty => Block ty size size
	-- |Matrix multiplication. Parameter's vector has dimensions isize*osize, rows (isize) first.
	MatMul :: NNVal ty => Block ty isize osize

blockInputSize :: Block ty isize osize -> Proxy isize
blockInputSize _ = Proxy

blockOutputSize :: Block ty isize osize -> Proxy osize
blockOutputSize _ = Proxy

paramsCount :: (KnownNat isize, KnownNat osize) => Block ty isize osize -> Int
paramsCount blk = case blk of
	Compose pca _ pcb _ -> pca + pcb
	Map _ -> 0
	PMap n _ -> n
	Shift -> inputSize
	MatMul -> inputSize * outputSize
	where
		inputSize = fromInteger $ natVal $ blockInputSize blk
		outputSize = fromInteger $ natVal $ blockOutputSize blk
