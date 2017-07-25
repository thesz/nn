-- |Types.hs
--
-- Different types' definitions.
--
-- Copyright (C) 2017 Serguey Zefirov

module Types where

import Data.Bits

import qualified Data.List as List

import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Word

newtype V = V { varIndex :: Int }
	deriving (Eq, Ord)

instance Show V where
	show (V x) = show x

newtype L = L { litIndex :: Int }
	deriving (Eq, Ord)

userToL :: Int -> L
userToL i = (if i < 0 then lnot else id) $ vToL $ userToV i

userToV :: Int -> V
userToV = V . abs

lnot :: L -> L
lnot = L . xor 1 . litIndex

lToV :: L -> V
lToV = V . flip shiftR 1 . litIndex

vToL :: V -> L
vToL = L . flip shiftL 1 . varIndex

instance Show L where
	show (L x)
		| odd x = "-"++show (div x 2)
		| otherwise = show (div x 2)



data C = C {
	  cLits		:: !LSet
	}

instance Show C where
	show (C ls) = List.intercalate "+" $ map show $ lsetToList ls

newtype VMap a = VMap { fromVMap :: IntMap.IntMap a }

instance Show a => Show (VMap a) where
	show (VMap m) = ("vmapFromList "++) $ show $ map (\(i,a) -> (V $ fromIntegral i, a)) $ IntMap.toList m

vmapToList :: VMap a -> [(V,a)]
vmapToList (VMap m) = map (\(v,a) -> (V $ fromIntegral v,a)) $ IntMap.toList m

newtype LMap a = LMap { fromLMap :: VMap (a,a) }

lmapToList :: LMap a -> [(L,a)]
lmapToList (LMap vmap) = concatMap (\(v,(p,n)) -> [(vToL v, p), (lnot $ vToL v,n)]) $ vmapToList vmap

newtype LSet = LSet { fromLSet :: LMap Bool }

lsetToList :: LSet -> [L]
lsetToList = map fst . filter snd . lmapToList . fromLSet