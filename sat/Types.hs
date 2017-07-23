-- |Types.hs
--
-- Different types' definitions.
--
-- Copyright (C) 2017 Serguey Zefirov

module Types where

import qualified Data.List as List

import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Word

newtype V = V { varIndex :: Word64 }
	deriving (Eq, Ord)

instance Show V where
	show (V x) = show x

newtype L = L { litIndex :: Word64 }
	deriving (Eq, Ord)

instance Show L where
	show (L x)
		| odd x = "-"++show (div x 2)
		| otherwise = show (div x 2)



data C = C {
	  cLits		:: !(Set.Set L)
	}

instance Show C where
	show (C ls) = List.intercalate "+" $ map show $ Set.toList ls

newtype VMap a = VMap { fromVMap :: IntMap.IntMap a }

instance Show a => Show (VMap a) where
	show (VMap m) = ("vmapFromList "++) $ show $ map (\(i,a) -> (V $ fromIntegral i, a)) $ IntMap.toList m

data Config = Config {
	  config
	}