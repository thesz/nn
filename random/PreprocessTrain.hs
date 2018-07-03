-- |PreprocessTrain.hs
--
-- Writes summary and individual counts of contexts and words to predict for training program to run on.
--
-- Copyright (C) Serguey Zefirov, 2018

{-# LANGUAGE BangPatterns, PatternGuards #-}

module PreprocessTrain(main) where

import Control.Monad

import qualified Data.List as List

import qualified Data.Map.Strict as Map

import qualified Data.Vector.Unboxed as UV

import System.Environment

import System.Exit

import System.IO

usage :: IO ()
usage = do
	prog <- getProgName
	putStrLn $ "usage: " ++ prog ++ " input-vector-wirdth output-vector-width ngram-order words-indices-fn train-corpus-fn preprocessed-train-corpus-output-fn"
	exitFailure

readInt :: String -> Maybe Int
readInt s = case reads s of
	((x,""):_) -> Just x
	_ -> Nothing

main :: IO ()
main = do
	args <- getArgs
	case args of
		[orderStr, wordsIndicesFN, trainCorpusFN, preprocessedOutputFN]
			| Just order <- readInt orderStr -> do
			wordsIndices <- readWordsIndices wordsIndicesFN
			counts <- count order wordsIndices trainCorpusFN 
			writeCounts preprocessedOutputFN counts
		_ -> usage

foldOverLines :: a -> Handle -> (String -> a -> a) -> IO a
foldOverLines !a0 h f = do
	eof <- hIsEOF h
	if eof
		then return a0
		else do
			l <- hGetLine h
			let	a = f l a0
			foldOverLines a h f

readWordsIndices :: String -> IO (Map.Map String Int)
readWordsIndices fn = do
	h <- openFile fn ReadMode
	indices <- foldOverLines Map.empty h $ \s map -> case words s of
			[w, ixStr]
				| Just ix <- readInt ixStr -> Map.insert w ix map
			_ -> error $ "file " ++ show fn ++ ": invalid line " ++ show s
	hClose h
	putStrLn $ "words' indices have been read."
	return indices

count :: Int -> Map.Map String Int -> String -> IO (Map.Map (UV.Vector Int) (Map.Map Int Int))
count order wordsIndices fn = do
	h <- openFile fn ReadMode
	cnts <- foldOverLines Map.empty h splitAddCounts
	hClose h
	return cnts
	where
		splitAddCounts s acc = Map.unionsWith (Map.unionWith (+)) $ acc : parts
			where
				order1 = order - 1
				ws = replicate (order1) "<s>" ++ words s ++ ["</s>"]
				find w = Map.findWithDefault (error $ "cannot find word " ++ show w ++ " from the line " ++ show s)
					w wordsIndices
				indices = map find ws
				vec = UV.fromList indices
				totalWords = UV.length vec
				numParts = totalWords - order1
				partsStarts = [0..numParts-1]
				parts = [uncurry Map.singleton (UV.force $ UV.slice i order1 vec, Map.singleton (vec UV.! (i + order1)) 1) | i <- partsStarts]

writeCounts :: String -> Map.Map (UV.Vector Int) (Map.Map Int Int) -> IO ()
writeCounts fn counts = do
	writeFile fn text
	putStrLn $ "counts have been written to "++fn
	where
		es = Map.toList counts
		contextWordsCounts (context, wordsCounts) =
			[ unwords $ map show $ UV.toList context
			, unwords $ concatMap wordCount $ Map.toList wordsCounts
			]
		wordCount (word, count) = [show word, show count]
		linesList = concatMap contextWordsCounts es
		text = unlines linesList
