-- |NGramLM.hs
--
-- Implement n-gram neural language model training and evaluation using NN.hs.
--
-- Copyright (C) Serguey Zefirov 2018

{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE PolyKinds, RankNTypes #-}

module NGramLM where

import NN

data NGramLM (contextSize :: Nat) (inputVectorSize :: Nat) (outputVectorSize :: Nat)

