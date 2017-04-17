-- |PenDigits.hs
--
-- Training pendigits problem with our cool framework.
--
-- (C) 2017 Serguey Zefirov

{-# LANGUAGE BangPatterns #-}

module PenDigits where

import Control.Monad

import Data.List (transpose)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Text.Printf
import Text.Show.Pretty

import E

trainFile = "pendigs/pendigits.tra"
testFile = "pendigs/pendigits.tes"

readInouts' :: String -> Int -> IO (NNData, NNData)
readInouts' fn nlines = do
	text <- readFile fn
	let	ils :: [[Int]]
		ils = map (read . ("["++) . (++"]")) $ (if nlines < 1 then id else take nlines) $ lines text
		flag n = [ if n == i then 1.0 else 0 | i <- [0..9]]
		outs = map (flag . last) ils
		ins = map (map (\i -> fromIntegral i / 100) . init) ils
		toNNData = V.fromList . (map UV.fromList) . transpose
	forM_ (zip ins outs) $ \(i,o) -> putStrLn $ "   "++show i++" -> "++show o
	return (toNNData ins, toNNData outs)

readInouts :: Int -> IO (NNData, NNData)
readInouts = readInouts' trainFile

readTestInouts = readInouts' testFile 0

testNN :: String -> NNet -> Map.Map Index Double -> IO ()
testNN nnName nn weights = do
	putStrLn $ "Testing "++nnName
	(testIns, testOuts) <- readTestInouts
	let	insToTest = map UV.fromList $ transpose $ map UV.toList $ V.toList testIns
		outsToCheck = map UV.fromList $ map (\xs -> map (fromEnum . (==maximum xs)) xs) $
				transpose $ map UV.toList $ V.toList testIns
		zeroes = UV.fromList (replicate 10 0 :: [Int])
		list = zip insToTest outsToCheck
--	forM_ list $ \(i,o) -> putStrLn $ "    "++show i++" -> "++show o
	loop zeroes zeroes list
	where
		loop :: UV.Vector Int -> UV.Vector Int -> [(UV.Vector Double, UV.Vector Int)] -> IO ()
		loop !countsEncountered !countsRight [] = do
			putStrLn "Testing statistics:"
			let	info = zip3 [0..] (UV.toList countsEncountered) (UV.toList countsRight)
			forM_ info $ \(d,n,r) -> let
					correct = fromIntegral r / fromIntegral n
				in printf "    digit %d: %5d/%5d (%5.3f)\n" (d :: Int) (r :: Int) (n :: Int) (correct :: Double)
		loop !countsEncountered !countsRight ((input,output):ios) = do
			let	outs' = V.map fromC $ nnEval weights input nn
				mx = maximum $ V.toList outs'
				outs :: UV.Vector Int
				outs = UV.map (fromEnum . (==mx)) $ UV.fromList $ V.toList outs'
				counts' = UV.zipWith (+) countsEncountered output
				rights' = UV.zipWith (+) countsRight $ UV.zipWith (*) outs output
			loop counts' rights' ios

trainPenDigits :: String -> NNet -> NNData -> NNData -> IO ()
trainPenDigits nnName nn inputs outputs = do
	putStrLn $ "Training "++nnName
	loop 40 1e20 (Map.map (const 0) $ nnIndices nn)
	where
		dumpWeights msg weights = do
			putStrLn $ "Stopped due to "++msg
			putStrLn $ "weights computed: "++show (Map.toList weights)
			testNN nnName nn weights
		loop 0 _ weights = dumpWeights "zero loop counter" weights
		loop n prevMinF currWeights = do
			putStrLn $ "  previous min func value: "++show prevMinF
			putStrLn $ "   current min func value: "++show currMinF
			putStrLn $ "current poly for min func: "++show (take takeN $ sToList minF)
			putStrLn $ "            current min t: "++show t
			putStrLn $ "            current delta: "++show delta
			if delta > min prevMinF currMinF * 0.01
				then loop (n-1) currMinF weights'
				else dumpWeights "convergence" currWeights
			where
				(logs, minF, weights) = construct currWeights inputs outputs nn
				(C c:_:C b:_:C a:_) = sToList minF
				t2 = negate b / (2*a)
				t = sqrt t2
				evalAtT s = sum ms
					where
						ts = take takeN $ iterate (*t) 1
						fromS (S (C x) ss) = x : fromS ss
						ms = zipWith (*) (fromS s) ts
				takeN = 5
				weights' = Map.map evalAtT weights
				currMinF = evalAtT minF
				delta = abs (prevMinF - currMinF)

		initialWeights :: Map.Map Index Double
		initialWeights = Map.map (const 0) $ nnIndices nn

simplePenDigsNN :: NNet
simplePenDigsNN = nnet True 16 [10]

twoLayerPenDigsNN :: NNet
twoLayerPenDigsNN = nnet True 16 [20,10]

testPenDigits name nn n = do
	(ins, outs) <- readInouts n
	trainPenDigits name nn ins outs

t = --testPenDigits "simple one fully connected layer NN" simplePenDigsNN 0
	testPenDigits "two layer NN" twoLayerPenDigsNN 200

main = t
