-- |Perceptron.hs
--
-- A model for perceptron for testing.
--
-- Copyright (C) 2017 Serguey Zefirov.

module Perceptron where

import E

import Text.Show.Pretty

perceptronNN :: [EE]
perceptronNN = nnet True 2 [2]

f :: Double -> Double -> Bool
f x y = y - 0.3 - x/2 > 0
