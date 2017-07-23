-- |Solver.hs
--
-- (Mostly) DPLL-style solver.
--
-- Copyright (C) Serguey Zefirov, 2017

module Solver where

import System.Environment (getArgs)

main = do
	args <- getArgs
	config <- parseArgs args emptyConfig
