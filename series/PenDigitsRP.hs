-- |PenDigits.hs
--
-- Training pendigits problem with our cool framework.
--
-- (C) 2017 Serguey Zefirov

{-# LANGUAGE BangPatterns #-}

module PenDigitsRP where

import Control.Monad

import Data.List (transpose, foldl1')

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.IO

import Text.Printf

import E

trainFile = "../pendigs/pendigits.tra"
testFile = "../pendigs/pendigits.tes"

rpOutputSize :: Int
rpOutputSize = 100

rpInputSize :: Int
rpInputSize = 16

rpTransformMatrix :: [[Double]]
rpTransformMatrix = ws
	where
		selThird x
			| x < 1/3 = negate 1/3
			| x > 2/3 = 1/3
			| otherwise = 0
		ws = map (map selThird) $ take rpOutputSize $ map (fst) $ tail $
			iterate (\(_,xs) -> splitAt rpInputSize xs) ([], threeGensRNG (112233.0 :: Double))

applyRP :: NNData -> NNData
applyRP nnd = V.fromList $ map app rpTransformMatrix
	where
		l = V.toList nnd
		selu x
			| x >= 0 = 1.0507 * x
			| otherwise = 1.0507 * 1.67326 * (exp x - 1)
		app cs = UV.map selu $ foldl1' (UV.zipWith (+)) $ zipWith (\v c -> UV.map (*c) v) l cs

readInouts' :: String -> Int -> IO (NNData, NNData)
readInouts' fn nlines = do
	text <- readFile fn
	let	ils :: [[Int]]
		ils = map (read . ("["++) . (++"]")) $ (if nlines < 1 then id else take nlines) $ lines text
		flag n = [ if n == i then 1.0 else -1.0 | i <- [0..9]]
		outs = map (flag . last) ils
		ins = map (map (\i -> 2*(fromIntegral i / 100)-1) . init) ils
		toNNData = V.fromList . (map UV.fromList) . transpose
--	forM_ (zip ins outs) $ \(i,o) -> putStrLn $ "   "++show i++" -> "++show o
	return (applyRP $ toNNData ins, toNNData outs)

readInouts :: Int -> IO (NNData, NNData)
readInouts = readInouts' trainFile

readTestInouts = readInouts' testFile 0

testNN :: String -> NNet -> Weights -> IO ()
testNN nnName nn weights = do
	testInsOuts <- readTestInouts
	testNN_ testInsOuts nnName nn weights

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
simplePenDigsNN = nnet True rpOutputSize [10]

twoLayerPenDigsNN :: NNet
twoLayerPenDigsNN = nnet True 16 [20,10]

testPenDigits name nn n = do
	(ins, outs) <- readInouts n
	trainPenDigits name nn ins outs

t =
	testPenDigits "simple one fully connected layer NN" simplePenDigsNN 0
	--testPenDigits "two layer NN" twoLayerPenDigsNN 0

main = do
	hSetBuffering stdout NoBuffering
	t
