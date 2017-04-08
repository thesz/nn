-- |E.hs
--
-- Expressions.

{-# LANGUAGE GADTs, RankNTypes, RecordWildCards #-}

module E where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map

import System.IO

type Index = [Int]

data E where
	-- |leaf terms - they do not contain references.
	Const	:: Double -> E
	Weight	:: Index -> E
	Bin	:: BinOp -> E -> E -> E
	Exp	:: E -> E
	deriving (Eq, Ord, Show)

data BinOp = Plus | Minus | Mul | Div
	deriving (Eq, Ord, Show)

instance Num E where
	a + b = Bin Plus a b
	a - b = Bin Minus a b
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

weight :: Index -> EE
weight index = EE (Weight index) (Map.singleton index 1)

softMax :: [EE] -> [EE]
softMax xs = map (\x -> exp x/denom) xs
	where
		denom = sum $ map exp xs

-- |The output is infinitely long.
layer :: Bool -> Int -> [EE] -> [EE]
layer addFree ln inputs = outs
	where
		add is
			| addFree = 1 : is
			| otherwise = is
		outs = [ o | j <- [0..], let o = sum [ e*weight [ln, i, j] | (i,e) <- zip [0..] $ add inputs]]

-- |Complete network activation.
nnet :: Bool -> [Double] -> [Int] -> [EE]
nnet addFree inputs sizes = f 1 eeinputs sizes
	where
		eeinputs = map constEE inputs
		f ln top [] = top
		f ln top (n : ns) = f (ln+1) (take n $ layer addFree ln top) ns

--------------------------------------------------------------------------------
-- Reading the problem.

pendigitsInputsCount = 16
pendigitsOutputsCount = 10

pendigits :: ([Double] -> [EE]) -> [Int] -> ([EE], [Double])
pendigits mk is = (nn, outputs)
	where
		inputs = map (fromInteger . fromIntegral) $ init is
		output = last is
		nn = mk inputs
		outputs = map (fromIntegral . fromEnum . (==output)) $ [0..pendigitsOutputsCount - 1]

pendigitsTrain :: ([Double] -> [EE]) -> [Int] -> EE
pendigitsTrain mk is = sum diff2s
	where
		(nn, outs) = pendigits mk is
		outEEs = map constEE outs
		diff2s = map (\x -> x*x) $ zipWith (-) nn outEEs

simplest, threeLayers :: [Double] -> [EE]
simplest is = nnet True is [pendigitsOutputsCount]

threeLayers is = nnet True is [30, 30, pendigitsOutputsCount]

readTrains :: Int -> IO EE
readTrains n = do
	text <- readFile "pendigs/pendigits.tra"
	let	ls = take n $ lines text
		bls = map (("["++) . (++"]")) ls
		wls :: [[String]]
		wls = map (words . map (\c -> if c == ',' then ' ' else c)) ls
		ints :: [[Int]]
		ints = map (map read) wls
		nns = map (pendigitsTrain simplest) ints
	return $ sum nns

initValue :: Index -> Double
initValue is = sum $ zipWith (\i j -> if odd i then j / 10 else j / 3) is [1..]

type PolyT = [Double]

integr :: Double -> PolyT -> PolyT
integr f0 ft = f0 : zipWith (/) ft [1..]

integration :: EE -> (Map.Map Index PolyT, PolyT)
integration (EE f partials) = (poss, eval f)
	where
		poss = Map.mapWithKey (\index -> integr $ initValue index) vels
		vels = Map.map (integr 0) accs
		accs = Map.map eval partials
		eval (Const c) = c : repeat 0
		eval (Weight i) = Map.findWithDefault (error $ "no position for "++show i++"???") i poss
		eval (Bin op a b) = case op of
			Plus -> zipWith (+) ap bp
			Minus -> zipWith (-) ap bp
			_ -> error $ "mul or div!"
			where
				ap = eval a
				bp = eval b
		eval (Exp e) = error "exp!!! aaaa!!!"


t = readTrains 2
