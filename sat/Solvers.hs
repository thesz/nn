-- |Solvers.hs
--
-- DPLL and stochastic solvers.
--
-- Copyright (C) Serguey Zefirov, 2017

{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Solver where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Data.Bits

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Word

import System.Environment (getArgs)

import Ops

dpllSolver :: [C] -> [L] -> [L] -> Int -> Int -> [C]
dpllSolver clauses fixed assignment nconflicts nrestarts =
	error "dpll!!!"
	where

data SSS = SSS {
	  sssUnsatClauses		:: Set.Set C
	, sssBestAssignment		:: Set.Set L
	}

newtype SSM a = SSM { runSSM :: State SSS a }
	deriving (Functor, Monad, Applicative)

stochasticSolver :: [Word64] -> [C] -> Int -> ([C], [L])
stochasticSolver randoms clauses iterations = (unsatisfied, resultingAssignment)
	where
		unsatisfied = error "unsat clauses!!!"
		resultingAssignment = error "resulting assignment!!!"

