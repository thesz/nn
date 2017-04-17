-- |Perceptron.hs
--
-- A model for perceptron for testing.
--
-- Copyright (C) 2017 Serguey Zefirov.

module Perceptron where

import Control.Monad

import Data.List (foldl')

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

reportTraining lc n = loop lc perceptronInitialWeights
	where
		dumpWeights ws = putStrLn $ "weights: "++show mx++" * "++show (Map.toList wss)
			where
				mx = Map.fold max 0 $ Map.map abs ws
				wss = Map.map (/mx) ws
		loop 0 currWeights = do
			putStrLn $ "Stopped by iteration count"
			dumpWeights currWeights
		loop n initial = do
			putStrLn $ "min function: "++ showS minF
			putStrLn $ "     t_min^2: "++ show t2
			putStrLn $ "       t_min: "++ show t
			forM_ (Map.toList weights) $ \(k,s) -> putStrLn $ "weight "++show k++": "++showS s
			if sumDiff > 0.01 then loop (n-1) initial' else do
				putStrLn $ "Stopped due to convergence"
				dumpWeights initial
			where
				(logs, minF, weights) = construct initial ins outs perceptronNN
				(C c:_:C b:_:C a:_) = sToList minF
				t2 = negate b / (2*a)
				t = sqrt t2
				evalAtT s = sum ms
					where
						ts = take takeN $ iterate (*t) 1
						fromS (S (C x) ss) = x : fromS ss
						ms = zipWith (*) (fromS s) ts
				initial' = Map.map evalAtT weights
				diff = Map.unionWith (\a b -> let x = a - b in x*x) initial initial'
				sumDiff = Map.foldl' (+) 0 diff
		takeN = 5
		showS = show . take takeN . sToList
		initial = perceptronInitialWeights
		(ins, outs) = perceptronInputsOutputs n

t = trainPerceptron 100
u x y = nnEval perceptronInitialWeights (UV.fromList [x,y]) perceptronNN
