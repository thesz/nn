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

import System.IO

import Text.Printf
import Text.Show.Pretty

import E

trainFile = "../pendigs/pendigits.tra"
testFile = "../pendigs/pendigits.tes"

readInouts' :: String -> Int -> IO (NNData, NNData)
readInouts' fn nlines = do
	text <- readFile fn
	let	ils :: [[Int]]
		ils = map (read . ("["++) . (++"]")) $ (if nlines < 1 then id else take nlines) $ lines text
		flag n = [ if n == i then 1.0 else -1.0 | i <- [0..9]]
		outs = map (flag . last) ils
		ins = map (map (\i -> fromIntegral i / 100) . init) ils
		toNNData = V.fromList . (map UV.fromList) . transpose
--	forM_ (zip ins outs) $ \(i,o) -> putStrLn $ "   "++show i++" -> "++show o
	return (toNNData ins, toNNData outs)

readInouts :: Int -> IO (NNData, NNData)
readInouts = readInouts' trainFile

readTestInouts = readInouts' testFile 0

testNN :: String -> NNet -> Weights -> IO ()
testNN nnName nn weights = do
	putStrLn $ "Testing "++nnName
	(testIns, testOuts) <- readTestInouts
	let	insToTest = map UV.fromList $ transpose $ map UV.toList $ V.toList testIns
		outsToCheck = map UV.fromList $ map (\xs -> map (fromEnum . (==maximum xs)) xs) $
				transpose $ map UV.toList $ V.toList testOuts
		zeroes = UV.fromList (replicate 10 0 :: [Int])
		list = zip insToTest outsToCheck
	--forM_ list $ \(i,o) -> putStrLn $ "    "++show i++" -> "++show o
	loop (UV.fromList $ replicate 10 0) zeroes zeroes list
	where
		loop :: UV.Vector Double -> UV.Vector Int -> UV.Vector Int -> [(UV.Vector Double, UV.Vector Int)] -> IO ()
		loop !sums !countsEncountered !countsRight [] = do
			putStrLn "Testing statistics:"
			let	info = zip (UV.toList sums) $ zip3 [0..] (UV.toList countsEncountered) (UV.toList countsRight)
			forM_ info $ \(s,(d,n,r)) -> let
					correct = fromIntegral r / fromIntegral n
				in printf "    digit %d: %5d/%5d (%5.3f), mean sum %8.3f\n" (d :: Int) (r :: Int) (n :: Int) (correct :: Double) s
			let	summary = fromIntegral (sum  $ UV.toList countsRight) / fromIntegral (sum $ UV.toList countsEncountered)
			printf "    summary error: %8.5f\n" (summary :: Double)
		loop !sums !countsEncountered !countsRight ((input,output):ios) = do
			let	outs' = V.map fromC $ nnEval weights input nn
				uouts = UV.fromList $ V.toList outs'
				mx = maximum $ V.toList outs'
				outs :: UV.Vector Int
				outs = UV.map (fromEnum . (==mx)) uouts
				counts' = UV.zipWith (+) countsEncountered output
				rights' = UV.zipWith (+) countsRight $ UV.zipWith (*) outs output
			loop (UV.zipWith (+) sums (UV.zipWith (*) uouts $ UV.map fromIntegral output)) counts' rights' ios

computeCorrectiveWeights :: Map.Map Index Double -> NNData -> NNData -> NNet -> NNData
computeCorrectiveWeights weights inputs expectedOutputs nn = corrWeights
	where
		realOutputs :: V.Vector (UV.Vector Double)
		realOutputs = nnEvalVec weights inputs nn
		veryNegativeNumber = -1e30
		-- expectedOutputs are either 0 or 1.
		-- here we compute values for expected class.
		valuesForExpected :: UV.Vector Double
		valuesForExpected = V.foldl1 max $ V.zipWith (UV.zipWith (\real exp -> if exp > 0 then real else veryNegativeNumber)) realOutputs expectedOutputs
		interestingCounts = V.foldl1 (UV.zipWith (+)) $ V.map (UV.zipWith (\exp out -> fromEnum $ out == exp) valuesForExpected) realOutputs
		nOuts = V.length realOutputs
		-- count must be at least 1 - a correct classification occurred.
		computeWeight count' expVal out
			| out == expVal = p+w1
			| out >= expVal = w1
			| otherwise = w0
			where
				count = --1
					count'
				p = 0.3
				w0 = (1-p) / fromIntegral (nOuts - count + nOuts*count)
				w1 = fromIntegral (nOuts -count) * w0
		corrWeights = V.map (UV.zipWith3 computeWeight interestingCounts valuesForExpected) realOutputs

trainPenDigits :: String -> NNet -> NNData -> NNData -> IO ()
trainPenDigits nnName nn inputs outputs = do
	ws <- trainClassifyLoop () nnName nn inputs outputs
	testNN nnName nn ws
	return ()

simplePenDigsNN :: NNet
simplePenDigsNN = nnet True 16 [10]

twoLayerPenDigsNN :: NNet
twoLayerPenDigsNN = nnet True 16 [20,10]

testPenDigits name nn n = do
	(ins, outs) <- readInouts n
	trainPenDigits name nn ins outs

t =
	--testPenDigits "simple one fully connected layer NN" simplePenDigsNN 0
	testPenDigits "two layer NN" twoLayerPenDigsNN 0

main = do
	hSetBuffering stdout NoBuffering
	t
