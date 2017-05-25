-- |AStar.hs
--
-- Minimum search using A* algorithm and interval arithmetic.
--
-- Copyright (C) 2017 Serguey Zefirov

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module AStar where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import I

class Split a where
	split :: a -> [a]

instance (Ord a, Fractional a) => Split (I a) where
	split (I l h) = [I l m, I m h]
		where
			m = (h+l)/2

instance (Ord a, Fractional a, Ord b, Fractional b) => Split (I a, I b) where
	split (a,b) = map (flip (,) b) (split a) ++ map ((,) a) (split b)

type Prio i = Map.Map (I Double) (Set.Set i)

astar :: (Ord i, Split i, Show i) => Double -> (i -> I Double) -> i -> IO i
astar eps f dom = go prio0
	where
		prio0 = Map.singleton (f dom) (Set.singleton dom)
		go prio
			| range mr < eps = do
				putStrLn $ "stopped - range check."
				putStrLn $ "Final result: "++show finalResult
				return finalResult
			| otherwise = do
				putStrLn $ "Min range: "++show mr
				go nextPrio
			where
				finalResult = head dsList
				dsList = Set.toList ds
				dsList' = concatMap split dsList
				minUpper = minimum $ map (high . fst) evald
				prioFiltered = Map.filterWithKey (\r _ -> low r > minUpper) prio'
				evald = map (\i -> (f i, Set.singleton i)) dsList'
				toAdd = Map.fromListWith Set.union $ filter ((<= minUpper) . low . fst) evald
				nextPrio = Map.unionWith Set.union toAdd prioFiltered
				Just ((mr,ds),prio') = Map.minViewWithKey prio

t = astar 0.001 (\r -> (r+1)*(r+1)) (I (-100) (100))