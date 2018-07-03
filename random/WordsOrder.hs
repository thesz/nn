-- |WordsOrder.hs
--
-- Read lines from input, count words' counts and assign their ordering based on their frequency.
--
-- The more frequent word, the smaller its index and shorter code.
--
-- Copyright (C) Serguey Zefirov, 2018

{-# LANGUAGE BangPatterns #-}

module WordsOrder(main) where

import qualified Data.List as List

import qualified Data.Map.Strict as Map

import System.Environment

import System.IO

main :: IO ()
main = do
	loop Map.empty

loop :: Map.Map String Int -> IO ()
loop !whole = do
	eof <- hIsEOF stdin
	if eof
		then dump whole
		else do
			line <- hGetLine stdin
			let	ws = "<s>" : "</s>" : words line
				counts = Map.unionsWith (+) $ map (`Map.singleton` 1) ws
			loop $ Map.unionWith (+) whole counts

dump :: Map.Map String Int -> IO ()
dump counts = do
	--mapM_ (\(word, cnt) -> hPutStrLn stderr $ unwords [word, show cnt]) $ Map.toList counts
	mapM_ (\(index,word) -> putStrLn $ unwords [word, show index]) withIndices
	where
		withIndices = zip [0..] $ map fst sorted
		sorted = reverse $ List.sortOn (uncurry $ flip (,)) $ Map.toList counts
