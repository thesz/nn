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

import Text.Show.Pretty

import System.IO

type Index = [Int]

data CV = C !Double | V (UV.Vector Double)
	deriving (Eq, Ord, Show)

data E where
	-- |leaf terms - they do not contain references.
	Const	:: CV -> E
	Input	:: Int -> E
	Weight	:: Index -> E
	Bin	:: BinOp -> E -> E -> E
	Exp	:: E -> E
	Sum	:: E -> E
	deriving (Eq, Ord, Show)

data BinOp = Plus | Minus | Mul | Div
	deriving (Eq, Ord, Show)

instance Num E where
	Const (C 0.0) + b = b
	a + Const (C 0.0) = a
	a + b = Bin Plus a b
	a - Const (C 0.0) = a
	a - b = Bin Minus a b
	Const (C 1.0) * b = b
	Const (C 0.0) * b = Const (C 0.0)
	a * Const (C 1.0) = a
	a * Const (C 0.0) = Const $ C 0.0
	Const c1 * Const c2 = Const $ cvMul c1 c2
	Const c1 * Bin Mul (Const c2) e = Const (cvMul c1 c2) * e
	Const c1 * Bin Mul e (Const c2) = Const (cvMul c1 c2) * e
	a * b = Bin Mul a b
	fromInteger = Const . C .  fromInteger
	negate =((Const $ C (-1))*)
	abs = error "no abs for E"
	signum = error "no signum for E"

instance Fractional E where
	fromRational = Const . C . fromRational
	a / b = Bin Div a b

instance Floating E where
	pi = Const $ C pi
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
		(Map.map (\da -> da / b)) (Map.map (\db -> negate $ db * a /(b*b))) da db)

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

constEEd :: Double -> EE
constEEd d = EE (Const $ C d) Map.empty

constEEv :: UV.Vector Double -> EE
constEEv v = EE (Const $ V v) Map.empty

inputEE :: Int -> EE
inputEE i = EE (Input i) Map.empty

sumEE :: EE -> EE
sumEE (EE e partials) = EE (Sum e) (Map.map Sum partials)

weight :: Index -> EE
weight index = EE (Weight index) (Map.singleton index 1)

softMax :: Floating e => [e] -> [e]
softMax xs = map (\x -> exp x/denom) xs
	where
		denom = hierSum $ map exp xs

softSat :: Floating e => e -> e
softSat x = let ex = exp x in ex / (1+ex)

softSats, softSigns :: Floating e => [e] -> [e]
softSats = map softSat

softSign :: Floating e => e -> e
softSign e = (1-e2e)/(1+e2e)
	where
		e2e = exp $ (-2)*e

softSigns = map softSign

-- |The output is infinitely long.
layer :: ([EE] -> [EE]) -> Bool -> Int -> [EE] -> [EE]
layer f addFree ln inputs = f outs
	where
		add is
			| addFree = 1 : is
			| otherwise = is
		outs = [ o | j <- [0..], let o = hierSum [ e*weight [ln, i, j] | (i,e) <- zip [0..] $ add inputs]]

type NNet = V.Vector EE

-- |Complete network activation.
nnet :: Bool -> Int -> [Int] -> NNet
nnet addFree inputs sizes = V.fromList $ f 1 eeinputs sizes
	where
		eeinputs = map inputEE [0..inputs-1]
		f ln top [] = top
		f ln top (n : ns)
			| null ns = f (ln+1) (take n $ layer softSigns addFree ln top) ns
			| otherwise = f (ln+1) (take n $ layer softSigns addFree ln top) ns

data S a = S !a (S a)

instance Functor S where
	fmap f (S a sa) = S (f a) (fmap f sa)

sZipWith :: (a -> b -> c) -> S a -> S b -> S c
sZipWith f (S a sa) (S b sb) = S (f a b) $ sZipWith f sa sb

sFrom :: Double -> S CV
sFrom a = S (C a) $ sFrom (a+1)

sToList :: S a -> [a]
sToList (S a sa) = a : sToList sa

type PolyT = S CV

pintegr :: CV -> PolyT -> PolyT
pintegr f0 ft = S f0 $ sZipWith cvDiv ft (sFrom 1)

pderiv :: PolyT -> PolyT
pderiv (S _ ft) = sZipWith cvMul ft (sFrom 1)

cvDiv, cvMul, cvAdd, cvSub :: CV -> CV -> CV
cvDiv (C 0) _ = C 0
cvDiv a (C 1) = a
cvDiv (C a) (C b) = C $ a / b
cvDiv (C a) (V b) = V $ UV.map (a/) b
cvDiv (V a) (C b) = V $ UV.map (/b) a
cvDiv (V a) (V b) = V $ UV.zipWith (/) a b

cvMul (C 0) _ = C 0
cvMul _ (C 0) = C 0
cvMul (C 1) b = b
cvMul a (C 1) = a
cvMul (C a) (C b) = C $ a * b
cvMul (C a) (V b) = V $ UV.map (a*) b
cvMul (V a) (C b) = V $ UV.map (*b) a
cvMul (V a) (V b) = V $ UV.zipWith (*) a b

cvAdd (C 0) b = b
cvAdd a (C 0) = a
cvAdd (C a) (C b) = C $ a + b
cvAdd (C a) (V b) = V $ UV.map (a+) b
cvAdd (V a) (C b) = V $ UV.map (+b) a
cvAdd (V a) (V b) = V $ UV.zipWith (+) a b

cvSub a (C 0) = a
cvSub (C a) (C b) = C $ a - b
cvSub (C a) (V b) = V $ UV.map (a-) b
cvSub (V a) (C b) = V $ UV.map (\a -> a-b) a
cvSub (V a) (V b) = V $ UV.zipWith (-) a b

cvExp :: CV -> CV
cvExp (C x) = C $ exp' x
cvExp (V y) = V $ UV.map exp' y

cvLog :: CV -> CV
cvLog (C x) = C $ log x
cvLog (V y) = V $ UV.map log y

pdiff :: PolyT -> PolyT
pdiff (S _ ft) = sZipWith cvMul (sFrom 1) ft

pscale :: CV -> PolyT -> PolyT
pscale c = fmap (cvMul c)

piscale :: CV -> PolyT -> PolyT
piscale c = fmap (flip cvDiv c)

pconst :: CV -> PolyT
pconst c = let zs = S (C 0) zs in S c zs

padd, psub, pmul, pdiv :: PolyT -> PolyT -> PolyT
padd = sZipWith cvAdd
psub = sZipWith cvSub
pmul (S a0 a) b@(S b0 bs) = S (cvMul a0 b0) (padd (pscale a0 bs) (pmul a b))
pdiv (S a0 a) b@(S b0 bs) = S c0  c
	where
		c0 = cvDiv a0 b0
		c = pdiv (psub a (pscale c0 bs)) b

exp' :: Double -> Double
exp' x = if abs x > 500 then exp (500 * signum x) else exp x

pexp :: PolyT -> PolyT
pexp u@(S u0 _) = w
	where
		svExp (C x) = C $ exp' x
		svExp (V v) = V $ UV.map exp' v
		w = pintegr (svExp u0) (pmul (pdiff u) w)

plog :: PolyT -> PolyT
plog u@(S u0 _) = w
	where
		w = pintegr (cvLog u0) (pdiv (pdiff u) w)

type CM a = State (Map.Map E PolyT)

type NNData = V.Vector (UV.Vector Double)

hierSum :: Num a => [a] -> a
hierSum [] = 0
hierSum [a] = a
hierSum xs = hierSum $ red xs
	where
		red (a:b:abs) = (a+b) : red abs
		red abs = abs

type Weights = Map.Map Index Double

construct :: Weights -> NNData -> NNData -> NNData -> NNet
	  -> (PolyT, Map.Map Index PolyT, Map.Map Index E)
construct initials inputsArrays outputsArrays correctiveWeights outputsExprs = (minFunc, weightsIntegr, partials)
	where
		nSamples = UV.length $ inputsArrays V.! 0
		scaleMul = --1.0 
			constEEd $ 1/(fromIntegral nSamples)
		EE goal partials = (*scaleMul) $ sumEE $ hierSum $ V.toList $ V.zipWith (*) (V.map constEEv correctiveWeights) $ V.zipWith (\out nnoutEE -> let x = constEEv out - nnoutEE in x*x) outputsArrays outputsExprs
		((logs,minFunc, weightsDerivs), allExpressionsAssigned) = flip runState Map.empty $ do
			(lmf,mf) <- liftM (\(l,v) -> (l, either pconst id v)) $ findAdd goal
			logsWeightsDerivs <- flip traverse partials $ \e -> liftM (\(l,v) -> (l,either pconst id v)) $ findAdd e
			return (lmf++concatMap fst (Map.elems logsWeightsDerivs), mf, Map.map snd logsWeightsDerivs)
		weightsVelocities = Map.map (pintegr (C 0) . pscale (C (-1))) weightsDerivs
		weightsIntegr = Map.intersectionWith (\init v -> pintegr (C init) v) initials weightsVelocities
		findAdd :: E -> State (Map.Map E (Either CV PolyT)) ([String], Either CV PolyT)
		findAdd e = do
			mbV <- liftM (Map.lookup e) get
			case mbV of
				Just v -> return ([],v)
				Nothing -> do
					(l,v) <- case e of
						Const cv -> return ([], Left cv)
						Input i -> return ([],Left . V $ inputsArrays V.! i)
						Weight i -> do
							let	w = Map.findWithDefault (error $ "unable to find weight "++show i) i weightsIntegr
							return ([], Right w)
						Bin op a b -> do
							(la,va) <- findAdd a
							(lb, vb) <- findAdd b
							return (lb++la,bin op va vb)
						Exp a -> do
							(l,v) <- findAdd a
							return (l,either (Left . cvExp) (Right . pexp) v)
						Sum e -> do
							(l,v) <- findAdd e
							let	f (C x) = C x
								f (V y) = C $ UV.foldl' (+) 0 y
							return (l,either (Left . f) (Right . fmap f) v)
					modify' $ Map.insert e v
					let	resV = either (\c -> "constant "++show c) (\s -> "poly with head "++show (take 1 (sToList s))) v
					return (l++["translating\n"++ppShow e, "result "++resV],v)
		add0 c (S b sb) = S (cvAdd b c) sb
		sub0 c (S b sb) = S (cvSub c b) (pscale (C (-1)) sb)
		bin Plus  (Left a)  (Left b)  = Left  $ cvAdd a b
		bin Plus  (Left a)  (Right b) = Right $ add0 a b
		bin Plus  (Right a) (Left b)  = Right $ add0 b a
		bin Plus  (Right a) (Right b) = Right $ padd a b
		bin Minus (Left a)  (Left b)  = Left  $ cvSub a b
		bin Minus (Left a)  (Right b) = Right $ sub0 a b
		bin Minus (Right a) (Left b)  = Right $ sub0 b a
		bin Minus (Right a) (Right b) = Right $ psub a b
		bin Mul   (Left a)  (Left b)  = Left  $ cvMul a b
		bin Mul   (Left a)  (Right b) = Right $ pscale a b
		bin Mul   (Right a) (Left b)  = Right $ pscale b a
		bin Mul   (Right a) (Right b) = Right $ pmul a b
		bin Div   (Left a)  (Left b)  = Left  $ cvDiv a b
		bin Div   (Left a)  (Right b) = Right $ pdiv (pconst a) b
		bin Div   (Right a) (Left b)  = Right $ piscale b a
		bin Div   (Right a) (Right b) = Right $ pdiv a b


eeIndices :: EE -> Map.Map Index ()
eeIndices (EE e _) = f e
	where
		f (Const _) = Map.empty
		f (Input _) = Map.empty
		f (Weight i) = Map.singleton i ()
		f (Bin _ a b) = Map.union (f a) (f b)
		f (Exp e) = f e
		f (Sum e) = f e

nnIndices :: V.Vector EE -> Map.Map Index ()
nnIndices = V.foldr Map.union Map.empty . V.map eeIndices

nnEval :: Weights -> UV.Vector Double -> NNet -> V.Vector CV
nnEval weights inputs nn = V.map eval nn
	where
		eval (EE e _) = f e
		f (Const c) = c
		f (Input i) = C $ inputs UV.! i
		f (Weight i) = C $ Map.findWithDefault (error $ "weight "++show i++" w/o default") i weights
		f (Bin op a b) = case op of
			Plus -> cvAdd ea eb
			Minus -> cvSub ea eb
			Mul -> cvMul ea eb
			Div -> cvDiv ea eb
			where
				ea = f a
				eb = f b
		f (Exp x) = cvExp $ f x
		f (Sum e) = error "there should not be sum!"

nnEvalVec :: Weights -> NNData -> NNet -> NNData
nnEvalVec weights inputs nn = V.map (check . eval) nn
	where
		check (C _) = error "get scalar in the nnEvalVec"
		check (V v) = v
		eval (EE e _) = f e
		f (Const c) = c
		f (Input i) = V $ inputs V.! i
		f (Weight i) = C $ Map.findWithDefault (error $ "weight "++show i++" w/o default") i weights
		f (Bin op a b) = case op of
			Plus -> cvAdd ea eb
			Minus -> cvSub ea eb
			Mul -> cvMul ea eb
			Div -> cvDiv ea eb
			where
				ea = f a
				eb = f b
		f (Exp x) = cvExp $ f x
		f (Sum e) = error "there should not be sum!"

fromC :: CV -> Double
fromC (C x) = x

findMinT :: S CV -> Double
findMinT s = undefined
	where
		l = sToList s
		n = 9

trainClassifyLoop :: () -> String -> NNet -> NNData -> NNData -> IO Weights
trainClassifyLoop computeScore nnName nn inputs outputs = do
	putStrLn $ "Training "++nnName
	loop True 40000 initialWeights
	where
		dumpWeights msg weights = do
			putStrLn $ "weights computed ("++msg++"): "++show (Map.toList weights)
			return weights
		loop first 0 weights = dumpWeights "zero loop counter" weights
		loop first n currWeights = do
			putStrLn $ "   train error percentage: "++show wrongsPercent
			putStrLn $ "  previous min func value: "++show prevMinF
			putStrLn $ "   current min func value: "++show currMinF
			putStrLn $ "current poly for min func: "++show (take takeN $ sToList minF)
			putStrLn $ "            current min t: "++show t
			putStrLn $ "            current delta: "++show delta
			if max prevMinF currMinF > 0.001 && delta > 0
				then loop False (n-1) weights'
				else dumpWeights "convergence" currWeights
			where
				currentOuts = nnEvalVec currWeights inputs nn
				mustMaxOuts :: UV.Vector Double
				mustMaxOuts = fst $ V.foldl1' (\(avs, aw) (bvs, bw) -> if aw > bw then (avs,aw) else (bvs,bw)) $
					V.zip currentOuts outputs
				countsAboveMustMax = V.foldl' (\cnts vs -> UV.zipWith (+) (cnts :: UV.Vector Double) $ UV.zipWith (\a b -> if a >= b then 1 else 0) vs mustMaxOuts)
					(UV.map (const 0) mustMaxOuts) currentOuts
				wrongs = UV.map (fromIntegral . fromEnum . (>1)) countsAboveMustMax
				wrongsPercent :: Double
				wrongsPercent = UV.sum wrongs * 100 / fromIntegral (UV.length wrongs)
				normMul = fromIntegral (UV.length countsAboveMustMax) / UV.sum countsAboveMustMax
				correctMuls = UV.map (*normMul) countsAboveMustMax
				(minF, weights, partials) = construct currWeights inputs outputs correctiveWeights nn
				maxF = 4.0
				S (C prevMinF) _ = minF
				computeMinT forMin s
					| abs c > th = if a == 0 then 0.01 else sqrt (abs c / abs a)
					| otherwise  = if a == 0 then 0.01 else sqrt (   th / abs a)
					where
						th = 0.001
						(C c':C b:C a:_) = sToList s
						c = signum c' * max 0.01 (abs c')
						smallStep = case (a < 0, b < 0) of
							(True,True) -> min (abs $ c/b) (sqrt $ abs $ c / a) / 20
							(False, True) -> if b == 0 then sqrt (c / a) / 5 else negate b / (2*a) / 2
							(True, False) -> if b == 0 then sqrt (negate c / a) / 5 else negate b / (2*a) / 2
							_ -> 0
				minFMinT = computeMinT True minF
				weightsStep = Map.foldl' (\t s -> min t $ computeMinT False s) minFMinT weights
				t = --(sqrt $ weightsStep * minFMinT) / 2 * (prevMinF / maxF)
					minFMinT / 10
				evalAtT s = sum ms
					where
						ts = take takeN $ iterate (*t) 1
						fromS (S (C x) ss) = x : fromS ss
						ms = zipWith (*) (fromS s) ts
				takeN = 3
				weights' = Map.map evalAtT weights
				currMinF = evalAtT minF
				delta = abs (prevMinF - currMinF)
				correctiveWeights =
					V.map (UV.map selectCW) outputs
					--V.map (UV.zipWith (*) correctMuls . UV.map selectCW) outputs
					--computeCorrectiveWeights currWeights inputs outputs nn

		outN = fromIntegral $ V.length nn
		alpha = 1/(2*outN-1)
		beta = outN*alpha
		selectCW w = if w > 0 then beta else alpha

		initialWeights :: Weights
		initialWeights = Map.mapWithKey (\k _ -> (if odd (sum k) then negate else id) $ fromIntegral (sum k)/1000) $ nnIndices nn
