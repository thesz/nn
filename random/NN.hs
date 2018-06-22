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

