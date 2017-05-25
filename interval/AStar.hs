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

astar :: (Ord i, Split i) => Double -> (i -> I Double) -> i -> IO i
astar eps f dom = go prio0
	where
		prio0 = Map.singleton (f dom) (Set.singleton dom)
		go prio
			| range i < eps = return $ head dsList
			| otherwise = undefined
			where
				dsList = Set.toList ds
				dsList' = concatMap split dsList
				
				Just ((i,ds),prio') = Map.minViewWithKey prio
