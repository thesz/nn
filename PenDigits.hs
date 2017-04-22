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

trainFile = "pendigs/pendigits.tra"
testFile = "pendigs/pendigits.tes"

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

testNN :: String -> NNet -> Map.Map Index Double -> IO ()
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
		interestingCounts = V.foldl1 (UV.zipWith (+)) $ V.map (UV.zipWith (\exp out -> fromEnum $ out >= exp) valuesForExpected) realOutputs
		nOuts = V.length realOutputs
		-- count must be at least 1 - a correct classification occurred.
		computeWeight count expVal out
			| out >= expVal = w1
			| otherwise = w0
			where
				w0 = 1 / fromIntegral (nOuts - count + nOuts*count)
				w1 = fromIntegral nOuts * w0
		corrWeights = V.map (UV.zipWith3 computeWeight interestingCounts valuesForExpected) realOutputs

trainPenDigits :: String -> NNet -> NNData -> NNData -> IO ()
trainPenDigits nnName nn inputs outputs = do
	putStrLn $ "Training "++nnName
	loop True 40000 initialWeights (Map.map (const 0) initialWeights)
	where
		dumpWeights stopped msg weights = do
			if stopped
				then putStrLn $ "Stopped due to "++msg
				else putStrLn $ "Testing the weights "++msg
			putStrLn $ "weights computed: "++show (Map.toList weights)
			testNN nnName nn weights
		loop first 0 weights vels = dumpWeights True "zero loop counter" weights
		loop first n currWeights currVels = do
			putStrLn $ "  previous min func value: "++show prevMinF
			putStrLn $ "   current min func value: "++show currMinF
			putStrLn $ "current poly for min func: "++show (take takeN $ sToList minF)
			putStrLn $ "            current min t: "++show t
			putStrLn $ "            current delta: "++show delta
			--putStrLn $ "       symb deriv [1,0,0]: "++show (Map.findWithDefault (error "!!!!!") [1,0,0] partials)
			--putStrLn $ "            deriv [1,0,0]: "++show (take takeN $ sToList $ Map.findWithDefault (error "!!!!!") [1,0,0] weights)
			--dumpWeights False "step start" currWeights
			dumpWeights False "step result" weights'
			if max prevMinF currMinF > 0.001 && delta > 0
				then loop False (n-1) weights' vels'
				else dumpWeights True "convergence" currWeights
			where
				(minF, weights, vels, partials) = construct currWeights currVels inputs outputs correctiveWeights nn
				S (C prevMinF) _ = minF
				computeMinT forMin s
					| abs c > th = if a == 0 then 0.01 else sqrt (abs c / abs a)
					| otherwise  = if a == 0 then 0.01 else sqrt (   th / abs a)
					where
						th = 0.001
						(C c:C b:C a:_) = sToList s
						smallStep = case (a < 0, b < 0) of
							(True,True) -> min (abs $ c/b) (sqrt $ abs $ c / a) / 20
							(False, True) -> if b == 0 then sqrt (c / a) / 5 else negate b / (2*a) / 2
							(True, False) -> if b == 0 then sqrt (negate c / a) / 5 else negate b / (2*a) / 2
							_ -> 0
				minFMinT = computeMinT True minF
				weightsStep = Map.foldl' (\t s -> min t $ computeMinT False s) minFMinT weights
				t = (weightsStep + minFMinT)/2/10
				evalAtT s = sum ms
					where
						ts = take takeN $ iterate (*t) 1
						fromS (S (C x) ss) = x : fromS ss
						ms = zipWith (*) (fromS s) ts
				takeN = 3
				weights' = Map.map evalAtT weights
				vels' = --Map.map evalAtT vels
					Map.map (const 0) vels
				currMinF = evalAtT minF
				delta = abs (prevMinF - currMinF)

		alpha = 1/19
		beta = 10*alpha
		selectCW w = if w > 0 then beta else alpha
		correctiveWeights = V.map (UV.map selectCW) outputs

		initialWeights :: Map.Map Index Double
		initialWeights = Map.mapWithKey (\k _ -> (if odd (sum k) then negate else id) $ fromIntegral (sum k)/1000) $ nnIndices nn

simplePenDigsNN :: NNet
simplePenDigsNN = nnet True 16 [10]

twoLayerPenDigsNN :: NNet
twoLayerPenDigsNN = nnet True 16 [20,10]

testPenDigits name nn n = do
	(ins, outs) <- readInouts n
	trainPenDigits name nn ins outs

t =
	--testPenDigits "simple one fully connected layer NN" simplePenDigsNN 0
	testPenDigits "two layer NN" twoLayerPenDigsNN 4000

main = do
	hSetBuffering stdout NoBuffering
	t
