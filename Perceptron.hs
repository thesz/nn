-- |Perceptron.hs
--
-- A model for perceptron for testing.
--
-- Copyright (C) 2017 Serguey Zefirov.

module Perceptron where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import E

import Text.Show.Pretty

perceptronNN :: V.Vector EE
perceptronNN = nnet True 2 [1]

f :: Double -> Double -> Bool
f x y = y - 0.3 - x/2 > 0

points :: [(Double, Double)]
points = concatMap Set.toList $ f Set.empty  pointSets
	where
		f p (a:as) = Set.difference a p : f a as
		pointSets = iterate newPoints initial
		initial = Set.fromList [(1,0), (0,1), (0,0), (1,1)]
		newPoints seenSet =
			Set.union seenSet $
			Set.fromList [ between p p' | p <- Set.toList seenSet, p' <- Set.toList seenSet, p /= p' ]
		between (a,b) (x,y) = ((a+x)/2,(b+y)/2)

perceptronInputsOutputs :: Int -> (NNData, NNData)
perceptronInputsOutputs n = (V.fromList [mkInput fst, mkInput snd], V.fromList [UV.fromList outputsList])
	where
		mkInput f = UV.fromList $ map f inputsList
		inputsList = take n points
		outputsList = map (fromIntegral . fromEnum . uncurry f) inputsList

perceptronInitialWeights :: Map.Map Index Double
perceptronInitialWeights = Map.map (const 0) $ nnIndices perceptronNN

--trainPerceptron :: Int -> Map.Map Index Double
trainPerceptron n = (logs, minF)
	where
		initial = perceptronInitialWeights
		(ins, outs) = perceptronInputsOutputs n
		(logs, minF, weights) = construct initial ins outs perceptronNN

t = trainPerceptron 100
u x y = nnEval perceptronInitialWeights (UV.fromList [x,y]) perceptronNN
