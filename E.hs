-- |E.hs
--
-- Expressions.

{-# LANGUAGE GADTs, RankNTypes, RecordWildCards #-}

module E where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.IO

type Index = [Int]

data E where
	-- |leaf terms - they do not contain references.
	Const	:: Double -> E
	Input	:: Int -> E
	Weight	:: Index -> E
	Bin	:: BinOp -> E -> E -> E
	Exp	:: E -> E
	Sum	:: E -> E
	deriving (Eq, Ord, Show)

data BinOp = Plus | Minus | Mul | Div
	deriving (Eq, Ord, Show)

instance Num E where
	Const 0.0 + b = b
	a + Const 0.0 = a
	a + b = Bin Plus a b
	a - Const 0.0 = a
	a - b = Bin Minus a b
	Const 1.0 * b = b
	Const 0.0 * b = Const 0.0
	a * Const 1.0 = a
	a * Const 0.0 = Const 0.0
	a * b = Bin Mul a b
	fromInteger = Const . fromInteger
	abs = error "no abs for E"
	signum = error "no signum for E"

instance Fractional E where
	fromRational = Const . fromRational
	a / b = Bin Div a b

instance Floating E where
	pi = Const pi
	exp = Exp
	log = error "no log for E"
	sin = error "no sin for E"
	cos = error "no cos for E"
	asin = error "no asin for E"
	acos = error "no acos for E"
	atan = error "no atan for E"
	sinh = error "no sinh for E"
	cosh = error "no cosh for E"
	asinh = error "no asinh for E"
	acosh = error "no acosh for E"
	atanh = error "no atanh for E"

data EE where
	EE :: E -> Map.Map Index E -> EE
	deriving (Eq, Ord, Show)

eFromEE :: EE -> E
eFromEE (EE e _) = e

instance Num EE where
	EE ea dsa + EE eb dsb = EE (ea + eb) (Map.unionWith (+) dsa dsb)
	EE a da - EE b db = EE (a - b) (Map.mergeWithKey (\_ a b -> Just $ a-b) id (Map.map negate) da db)
	EE a da * EE b db = EE (a * b) (Map.unionWith (+) (Map.map (a*) db) (Map.map (b*) da))
	fromInteger x = EE (fromInteger x) Map.empty
	abs = error "no abs for E"
	signum = error "no signum for E"

instance Fractional EE where
	fromRational x = EE (fromRational x) Map.empty
	EE a da / EE b db = EE (a/b) (Map.mergeWithKey (\_ da db -> Just $ da / b - db * a / (b*b))
		(Map.map (\da -> da / b)) (Map.map (\db -> db * a /(b*b))) da db)

instance Floating EE where
	pi = EE pi Map.empty
	exp (EE a da) = EE (exp a) (Map.map (exp a*) da)
	log = error "no log for E"
	sin = error "no sin for E"
	cos = error "no cos for E"
	asin = error "no asin for E"
	acos = error "no acos for E"
	atan = error "no atan for E"
	sinh = error "no sinh for E"
	cosh = error "no cosh for E"
	asinh = error "no asinh for E"
	acosh = error "no acosh for E"
	atanh = error "no atanh for E"

constEE :: Double -> EE
constEE d = EE (Const d) Map.empty

inputEE :: Int -> EE
inputEE i = EE (Input i) Map.empty

sumEE :: EE -> EE
sumEE (EE e partials) = EE (Sum e) (Map.map Sum partials)

weight :: Index -> EE
weight index = EE (Weight index) (Map.singleton index 1)

softMax :: Floating e => [e] -> [e]
softMax xs = map (\x -> exp x/denom) xs
	where
		denom = sum $ map exp xs

softSat :: Floating e => e -> e
softSat x = let ex = exp x in ex / (1+ex)

-- |The output is infinitely long.
layer :: Bool -> Int -> [EE] -> [EE]
layer addFree ln inputs = outs
	where
		add is
			| addFree = 1 : is
			| otherwise = is
		outs = [ o | j <- [0..], let o = sum [ e*weight [ln, i, j] | (i,e) <- zip [0..] $ add inputs]]

-- |Complete network activation.
nnet :: Bool -> Int -> [Int] -> [EE]
nnet addFree inputs sizes = map softSat $ f 1 eeinputs sizes
	where
		eeinputs = map inputEE [0..inputs-1]
		f ln top [] = top
		f ln top (n : ns) = f (ln+1) (take n $ layer addFree ln top) ns

data S a = S !a (S a)

instance Functor S where
	fmap f (S a sa) = S (f a) (fmap f sa)

sZipWith :: (a -> b -> c) -> S a -> S b -> S c
sZipWith f (S a sa) (S b sb) = S (f a b) $ sZipWith f sa sb

sFrom :: Double -> S CV
sFrom a = S (C a) $ sFrom (a+1)

sToList :: S a -> [a]
sToList (S a sa) = a : sToList sa

data CV = C !Double | V (UV.Vector Double)
	deriving (Eq, Ord, Show)

type PolyT = S CV

pintegr :: CV -> PolyT -> PolyT
pintegr f0 ft = S f0 $ sZipWith svDiv ft (sFrom 1)

svDiv, svMul, svAdd, svSub :: CV -> CV -> CV
svDiv (C a) (C b) = C $ a / b
svDiv (C a) (V b) = V $ UV.map (a/) b
svDiv (V a) (C b) = V $ UV.map (/b) a
svDiv (V a) (V b) = V $ UV.zipWith (/) a b

svMul (C a) (C b) = C $ a * b
svMul (C a) (V b) = V $ UV.map (a*) b
svMul (V a) (C b) = V $ UV.map (*b) a
svMul (V a) (V b) = V $ UV.zipWith (*) a b

svAdd (C a) (C b) = C $ a + b
svAdd (C a) (V b) = V $ UV.map (a+) b
svAdd (V a) (C b) = V $ UV.map (+b) a
svAdd (V a) (V b) = V $ UV.zipWith (+) a b

svSub (C a) (C b) = C $ a - b
svSub (C a) (V b) = V $ UV.map (a-) b
svSub (V a) (C b) = V $ UV.map (\a -> a-b) a
svSub (V a) (V b) = V $ UV.zipWith (-) a b

pdiff :: PolyT -> PolyT
pdiff (S _ ft) = sZipWith svMul (sFrom 1) ft

pscale :: CV -> PolyT -> PolyT
pscale c = fmap (svMul c)

padd, psub, pmul, pdiv :: PolyT -> PolyT -> PolyT
padd = sZipWith svAdd
psub = sZipWith svSub
pmul (S a0 a) b@(S b0 bs) = S (svMul a0 b0) (padd (pscale a0 bs) (pmul a b))
pdiv (S a0 a) b@(S b0 bs) = S c0  c
	where
		c0 = svDiv a0 b0
		c = pdiv (psub a (pscale c0 bs)) b

pexp :: PolyT -> PolyT
pexp u@(S u0 _) = w
	where
		exp' x = if abs x > 500 then exp (500 * signum x) else exp x
		svExp (C x) = C $ exp' x
		svExp (V v) = V $ UV.map exp' v
		w = pintegr (svExp u0) (pmul (pdiff u) w)

type CM a = State (Map.Map E PolyT)

construct :: Map.Map Index Double -> V.Vector (UV.Vector Double) -> V.Vector (UV.Vector Double) -> V.Vector EE -> (PolyT, Map.Map Index PolyT)
construct initials inputsArrays outputsAraays outputsExprs = undefined
	where
		(minFunc, allExpressions) = flip runState undefined $ do
			undefined

integration :: (Index -> Double) -> (Int -> UV.Vector Double) -> EE -> (Map.Map Index PolyT, PolyT)
integration initValue input (EE f partials) = (poss, eval f)
	where
		poss = Map.mapWithKey (\index -> pintegr $ C $ initValue index) vels
		vels = Map.map (pintegr $ C 0) accs
		accs = Map.map (pscale (C $ -1) . eval) partials
		eval (Const c) = let zs = S (C 0) zs in S (C c) zs
		eval (Input i) = let zs = S (C 0) zs in S (V $ input i) zs
		eval (Weight i) = Map.findWithDefault (error $ "no position for "++show i++"???") i poss
		eval (Bin op a b) = case op of
			Plus -> padd ap bp
			Minus -> psub ap bp
			Mul -> pmul ap bp
			Div -> pdiv ap bp
			where
				ap = eval a
				bp = eval b
		eval (Sum e) = let ee = eval e in fmap cvSum ee
		eval (Exp e) = pexp $ eval e
		cvSum (C x) = C x
		cvSum (V y) = C $ UV.sum y

