-- |F.hs
--
-- A successor to E.hs. ;)
--
-- Copyright (C) Serguey Zefirov, 2018.

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module F where

import Control.Applicative hiding (Const)

import Control.Monad

import Data.Bits

import qualified Data.List as List

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Data.Word

data E input param =
		Const	!Word64	!Double
	|	Param	!Word64	param
	|	Input	!Word64	input
	|	Bin	!Word64	!BinOp !(E input param) !(E input param)
	|	Un	!Word64	!UnOp !(E input param)
	deriving (Eq, Ord)

instance (Show input, Show param) => Show (E input param) where
	showsPrec p e = case e of
		Const _ x -> showsPrec p x
		Param _ param -> showParen (p>=10) (\s -> "mkParam " ++ showsPrec 11 param s)
		Input _ input -> showParen (p>=10) (\s -> "mkInput " ++ showsPrec 11 input s)
		Bin _ op a b -> case op of
			Plus -> showParen (p>=6) $ showsPrec 6 a . (" + "++) . showsPrec 6 b
			Minus -> showParen (p>=6) $ showsPrec 6 a . (" - "++) . showsPrec 6 b
			Mul -> showParen (p>=7) $ showsPrec 7 a . (" * "++) . showsPrec 7 b
			Div -> showParen (p>=7) $ showsPrec 7 a . (" / "++) . showsPrec 7 b
		Un _ op a -> showParen (p>=10) (\s -> name ++ " " ++ showsPrec 11 a s)
			where
				name = case op of
					Negate -> "negate"
					Exp -> "exp"
					Log -> "log"
					Sqrt -> "sqrt"
					Sum -> "mkSum"

changeInput :: (Hashed input1, Hashed input2, Ord input1, Ord input2, Ord param) => (input1 -> input2) -> E input1 param -> E input2 param
changeInput f (Input _ i) = mkInput $ f i
changeInput f (Bin _ op a b) = case op of
	Plus -> ca + cb
	Minus -> ca - cb
	Mul -> ca * cb
	Div -> ca / cb
	where
		ca = changeInput f a
		cb = changeInput f b
changeInput f (Un _ op x) = case op of
	Negate -> negate cx
	Exp -> exp cx
	Log -> log cx
	Sqrt -> sqrt cx
	Sum -> mkSum cx
	where
		cx = changeInput f x
changeInput f (Const h c) = Const h c
changeInput f (Param h p) = Param h p

data BinOp = Plus | Minus | Mul | Div
	deriving (Eq, Ord, Show)

data UnOp = Negate | Exp | Log | Sqrt | Sum
	deriving (Eq, Ord, Show)

combineHash :: Word64 -> Word64 -> Word64 -> Word64
combineHash seed a b = r
	where
		r = f (rotateL seed 19) $ f (rotateL b 24) $ f (rotateL a 32) $ f b $ f a seed
		f a p = xor x a
			where
				(pl, ph) = spl $ rotateL p 12
				x = rotateL (pl * 204720472047 + ph * 24101971) 17
		spl a = (a .&. (shiftL 1  32 - 1), shiftR a 32)
		(al, ah) = spl (rotateL (2047 * a + 1971 * seed) 17)
		(bl, bh) = spl b
		abll = al*bl
		ablh = al*bh

mkConst :: Double -> E input param
mkConst dbl = Const h dbl
	where
		(mant, exp) = decodeFloat dbl
		h = combineHash 1122334455 (fromIntegral mant) (fromIntegral exp)

class Hashed a where
	getHash :: a -> Word64

instance Hashed (E input param) where
	getHash (Const h _) = h
	getHash (Param h _) = h
	getHash (Input h _) = h
	getHash (Bin h _ _ _) = h
	getHash (Un h _ _) = h

merkle :: Word64 -> [Word64] -> Word64
merkle root [] = root
merkle root [x] = combineHash 1001001001001 root x
merkle root xs = merkle (root + 2003004005006007) $ join xs
	where
		join (a:b:abs) = combineHash root a b : join abs
		join abs = abs

instance Hashed String where
	getHash s = merkle 12312312223 $ map (fromIntegral . fromEnum) s
instance {-# OVERLAPPABLE #-} Integral a => Hashed [a] where
	getHash s = merkle 12312312223 $ map fromIntegral s

instance Hashed Bool where
	getHash False = 1234567890123457
	getHash True  = 5678901234567890

instance Hashed () where
	getHash _ = getHash "() - unit"

instance (Hashed a, Hashed b) => Hashed (Either a b) where
	getHash (Left a) = combineHash (getHash "Left") (getHash a) 1
	getHash (Right b) = combineHash (getHash "Right") (getHash b) 2

-- same as Either, but more domain-specific.
data InOut input output = In input | Out output
	deriving (Eq, Ord, Show)

instance (Hashed i, Hashed o) => Hashed (InOut i o) where
	getHash (In i) = combineHash (getHash "In") (getHash i) 1
	getHash (Out o) = combineHash (getHash "Out") (getHash o) 2

mkParam :: Hashed param => param -> E input param
mkParam p = Param (getHash p) p

mkInput :: Hashed input => input -> E input param
mkInput i = Input (getHash i) i

mkSum :: (Ord input, Ord param) => E input param -> E input param
mkSum e = case e of
	Const _ x
		| x == 0.0 -> mkConst 0.0
--	Bin _ Plus a b -> mkSum a + mkSum b
--	Bin _ Minus a b -> mkSum a - mkSum b
	Bin _ Mul c@(Const _ _) e -> c * mkSum e
	_ -> Un (combineHash (getHash "sum") (getHash e) (getHash "extra")) Sum e

instance (Ord param, Ord input) => Num (E input param) where
	Const _ 0.0 + e = e
	e + Const _ 0.0 = e
	Const _ a + Const _ b = mkConst $ a + b
	Un _ Sum a + Un _ Sum b = mkSum $ a + b
	a + b
		| a < b = Bin (combineHash 223344556677 (getHash a) (getHash b)) Plus a b
		| a == b = 2 * a
		| otherwise = b + a

	Const _ 0.0 - e = negate e
	e - Const _ 0.0 = e
	Const _ a - Const _ b = mkConst $ a - b
	Un _ Sum a - Un _ Sum b = mkSum $ a - b
	a - b = Bin (combineHash 12342345345 (getHash a) (getHash b)) Minus a b

	Const _ 1.0 * e = e
	e * Const _ 1.0 = e
	Const _ (-1.0) * e = negate e
	e * Const _ (-1.0) = negate e
	Const h 0.0 * e = Const h 0.0
	e * Const h 0.0 = Const h 0.0
	Const _ a * Const _ b = mkConst $ a * b
	Const _ a * Bin _ Mul (Const _ b) c = (mkConst $ a * b) * c
	Const _ a * Bin _ Div (Const _ b) c = (mkConst $ a * b) / c
	a@(Const _ _) * Bin _ Plus b@(Const _ _) c = a * b + a * c
	a@(Const _ _) * Bin _ Minus b@(Const _ _) c = a * b - a * c
	a * b
		| a <= b = Bin (combineHash 334455667789 (getHash a) (getHash b)) Mul a b
		| otherwise = b * a

	negate (Un _ Negate e) = e
	negate (Const _ x) = mkConst (negate x)
	negate (Bin _ Mul a@(Const _ _) b) = negate a * b
	negate (Bin _ Div a@(Const _ _) b) = negate a / b
	negate e@(Bin _ Div a b)
		| a == b = 1
		| otherwise = e
	negate e = Un (combineHash 9900000099 (getHash e) 10000000001) Negate e

	abs _ = error "no abs for E"
	signum = error "no signum for E"
	fromInteger = mkConst . fromInteger

instance (Ord param, Ord input) => Fractional (E input param) where
	fromRational = mkConst . fromRational
	Const _ 0.0 / e = mkConst 0
	e / Const _ 1 = e
	e / Const _ x = (mkConst (1/x)) * e
	a / b = Bin (combineHash 98765432101 (getHash a) (getHash b)) Div a b
	recip a = 1 / a

instance (Ord param, Ord input) => Floating (E input param) where
	pi = mkConst pi
	sqrt (Const _ e) = mkConst $ sqrt e
	sqrt e = Un (combineHash 709070809033 (getHash e) 10101010202020) Sqrt e
	exp (Const _ e) = mkConst $ exp e
	exp e = Un (combineHash 909090801045 (getHash e) 22222333334444) Exp e
	log (Const _ e) = mkConst $ log e
	log e = Un (combineHash 909090801045 (getHash e) 22222333334444) Log e
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

sigmoid :: Floating x => x -> x
sigmoid x = 1/(exp (negate x) + 1)

softSign :: Floating x => x -> x
softSign x = (1 - e2x) / (1 + e2x)
	where
		e2x = exp ((-2)*x)

derivatives :: (Ord param, Ord input) => E input param -> Map.Map param (E input param)
derivatives (Const _ _) = Map.empty
derivatives (Param _ param) = Map.singleton param 1
derivatives (Input _ _) = Map.empty
derivatives e@(Bin _ op a b) = case op of
	Plus -> Map.mergeWithKey (chk (+)) id id da db
	Minus -> Map.mergeWithKey (chk (-)) id (Map.map negate) da db
	Mul -> Map.mergeWithKey (chk (+)) id id bda adb
		where
			adb = Map.map (*a) db
			bda = Map.map (*b) da
	Div -> Map.mergeWithKey (chk (-)) id id dab dbe
		where
			dab = Map.map (/b) da
			dbe = Map.map (\db -> e*db/b) db
	where
		chk f _ l r = case z of
			Const _ x
				| x == 0.0 -> Nothing
			_ -> Just z
			where
				z = f l r
		da = derivatives a
		db = derivatives b
derivatives x@(Un _ op a) = case op of
	Negate -> Map.map negate da
	Exp -> Map.map (*x) da
	Log -> Map.map (/a) da
	Sqrt -> Map.map ((*0.5) . (/x)) da
	Sum -> Map.map mkSum da
	where
		da = derivatives a

data S e = S e (S e)

instance Functor S where
	fmap f (S e se) = S (f e) $ fmap f se

sToList :: S e -> [e]
sToList (S e se) = e : sToList se

showS :: Show e => Int -> S e -> String
showS n = (++"...") . init . show . take n . sToList

sZeroes :: Num e => S e
sZeroes = S 0 sZeroes

sConst :: Num e => e -> S e
sConst e = S e sZeroes

sScale :: Num e => e -> S e -> S e
sScale c = fmap (*c)

sZip :: (a -> b -> c) -> S a -> S b -> S c
sZip f (S a sa) (S b sb) = S (f a b) $ sZip f sa sb

instance Num e => Num (S e) where
	fromInteger = sConst . fromInteger
	(+) = sZip (+)
	(-) = sZip (-)
	negate = fmap negate
	S a sa * sb@(S b sb') = S (a*b) $ sa * sb + sScale a sb'
	abs = error "no abs for S"
	signum = error "no signum for S"

sIntegr :: Fractional e => e -> S e -> S e
sIntegr c0 s = let is = S 1 $ fmap (+1) is in S c0 $ sZip (/) s is

sDiff :: Fractional e => S e -> S e
sDiff (S _ s) = let is = S 1 $ fmap (+1) is in sZip (*) s is

instance Fractional e => Fractional (S e) where
	fromRational = sConst . fromRational
	recip s@(S x xs) = S c $ negate (sScale c xs) / s
		where
			c = 1/x
	(S a0 a) / b@(S b0 bs) = S c0  c
		where
			c0 = a0 / b0
			c = (a - sScale c0 bs) / b

instance Floating e => Floating (S e) where
	pi = sConst pi
	sqrt se = error "sqrt S"
	exp s@(S e se) = let w = sIntegr (exp e) (sDiff s * w) in w
	log s@(S e se) = let w = sIntegr (log e) (sDiff s / s) in w
	sin = error "no sin for S"
	cos = error "no cos for S"
	asin = error "no asin for S"
	acos = error "no acos for S"
	atan = error "no atan for S"
	sinh = error "no sinh for S"
	cosh = error "no cosh for S"
	asinh = error "no asinh for S"
	acosh = error "no acosh for S"
	atanh = error "no atanh for S"

paramsIntegration :: forall input param . (Hashed param, Ord input, Ord param) => E input param -> (S (E input param), Map.Map param (S (E input param)))
paramsIntegration expr = (evalUnderParams expr, paramsIntegrs)
	where
		derivs = derivatives expr
		paramsIntegrs :: Map.Map param (S (E input param))
		paramsIntegrs = Map.mapWithKey (\param d -> sIntegr (mkParam param) $ sIntegr 0.0 $ negate $ evalUnderParams d) derivs
		evalUnderParams :: E input param -> S (E input param)
		evalUnderParams e = case e of
			Const _ _ -> sConst e
			Param _ param -> Map.findWithDefault (error "can't find some param!!!") param paramsIntegrs
			Input	_ _ -> sConst e
			Bin _ op a b -> case op of
				Plus -> ea + eb
				Minus -> ea - eb
				Mul -> ea * eb
				Div -> ea / eb
				where
					ea = evalUnderParams a
					eb = evalUnderParams b
			Un _ op x -> case op of
				Negate -> negate ex
				Exp -> exp ex
				Log -> log ex
				Sqrt -> sqrt ex
				Sum -> fmap mkSum ex
				where
					ex = evalUnderParams x
exprToMap e = case e of
	Bin _ _ a b -> Map.insert e () $ Map.union (exprToMap a) (exprToMap b)
	Un _ _ a -> Map.insert e () $ exprToMap a
	_ -> Map.singleton e ()

exprUsesMap x e = case e of
	Bin _ _ a b -> Map.insert a x $ Map.singleton b x
	Un _ _ a -> Map.singleton e x
	_ -> Map.empty

trainAllExpressionsMap :: (Hashed input, Hashed output, Ord input, Ord output, Hashed param, Ord param) =>
	Int -> S (E (InOut input output) param) -> Map.Map param (S (E (InOut input output) param)) ->
		([E (InOut input output) param], Map.Map param [(E (InOut input output) param)], Map.Map (E (InOut input output) param) ())
trainAllExpressionsMap numCoefs exprCoefs accels = (exprCoefsList, accelsLists, allExprs)
	where
		allExprs = Map.unions (map exprToMap exprCoefsList ++[Map.foldr (\l es -> Map.union es $ Map.unions $ map exprToMap l) Map.empty accelsLists])
		sCoefs = take numCoefs . sToList
		sToMap s = Map.unions $ map exprToMap $ sCoefs s
		exprCoefsList = sCoefs exprCoefs
		accelsLists = Map.map sCoefs accels

trainGetSums :: (Hashed input, Hashed output, Ord input, Ord output, Hashed param, Ord param, Show input, Show output, Show param) =>
	Map.Map param [E (InOut input output) param] -> [E (InOut input output) param] -> IO [[E (InOut input output) param]]
trainGetSums integrs optExpr = do
--	putStrLn $ "number of sums as roots: "++show (Map.size rootSums)
--	forM_ (Map.toList rootSums) $ \(a,b) -> do
--		putStrLn $ "    a "++show a
--	putStrLn $ "number of expressions to compute root sums: "++ show (Map.size rootSumsExprs)
--	putStrLn $ "number of complex expressions to compute root sums: "++ show (Map.size goodToCompute)
--	forM_ (Map.toList goodToCompute) $ \(a,b) -> do
--		putStrLn $ "    a "++show a
--	putStrLn $ "front sizes: "++show (map length fronts)
	return fronts
	where
		exprRootSums e = case e of
			Un _ Sum _ -> Map.singleton e ()
			Un _ _ e -> exprRootSums e
			Bin _ _ a b -> Map.union (exprRootSums a) (exprRootSums b)
			_ -> Map.empty
		rootSums = rootSumsList optExpr `Map.union` Map.foldr (\l rs -> Map.union rs $ rootSumsList l) Map.empty integrs
		rootSumsList = Map.unions . map exprRootSums
		rootSumsExprs = Map.foldr Map.union rootSums $ Map.mapWithKey (\rsum _ -> exprToMap rsum) rootSums
		usesCounts = Map.foldr (Map.unionWith (+)) Map.empty $ Map.mapWithKey (\e _ -> exprUsesMap 1 e) rootSumsExprs
		notSimple (Param _ _) = False
		notSimple (Input _ _) = False
		notSimple (Const _ _) = False
		notSimple _           = True
		mustBeInFront cnt e = case e of
			Un _ Sum _ -> True
			Param _ _ -> False
			Input _ _ -> False
			Const _ _ -> False
			_ -> False
		goodToCompute = Map.union rootSums $ Map.map (const ()) $ Map.filterWithKey (\e cnt -> mustBeInFront cnt e) usesCounts
		canBeComputed alreadyComputed e
			| Map.member e alreadyComputed = True
			| otherwise = case e of
				Bin _ _ a b -> canBeComputed alreadyComputed a && canBeComputed alreadyComputed b
				Un _ Sum _ -> False
				Un _ _ a -> canBeComputed alreadyComputed a
				_ -> True
		computeFront alreadyComputed = filter f
			where
				f (Un _ Sum a) = canBeComputed alreadyComputed a
				f (Bin _ _ a b) = canBeComputed alreadyComputed a && canBeComputed alreadyComputed b
				f _ = True -- trivial
		fronts = computeFronts Map.empty goodToCompute
		computeFronts computed remaining
			| Map.null remaining = []
			| null front = []
			| otherwise = front : computeFronts (Map.union computed frontMap) (Map.difference remaining frontMap)
				where
					front = computeFront computed $ Map.keys remaining
					frontMap = Map.fromList $ map (flip (,) ()) front

trainStep :: (Hashed input, Hashed param, Ord input, Ord param, Ord output, Hashed output, Show input, Show output, Show param) => Int -> Int -> E (InOut input output) param -> Map.Map input (UV.Vector Double) -> Map.Map output (UV.Vector Double) -> Map.Map param Double -> IO (Maybe (Map.Map param Double))
trainStep sliceSize threads trainExpr inputs outputs initialParams = do
--	print trainExpr
--	print trainExprCoefsList
	when (Map.null outputs) $ error "can't train with empty outputs"
	when (not vectorsAreGood) $ error "not all inputs and outputs are of same length"
--	putStrLn $ "number of different expressions "++show (Map.size expressions)
	stages <- trainGetSums paramsCoefsLists trainExprCoefsList
--	forM_ stages $ \stage -> do
--		putStrLn $ "stage:"
--		forM_ stage $ \e -> putStrLn $ "    "++show e
--	forM_ (Map.keys expressions) $ \e ->
--		putStrLn $ "--- "++show e
	let	initialScalars = Map.foldr Map.union Map.empty $ Map.mapWithKey (\p v -> Map.singleton (mkParam p) v) initialParams
--	putStrLn $ "initial scalars: "++show initialScalars
	computeScalarsLoop initialScalars stages
	where
		allVectors = Map.elems inputs ++ Map.elems outputs
		vectorSize = UV.length $ head allVectors
		vectorsAreGood = all ((vectorSize ==) . UV.length) allVectors
		(trainExprCoefsList, paramsCoefsLists, expressions) = trainAllExpressionsMap 5 trainExprCoefs derivatives
		(trainExprCoefs, derivatives) = paramsIntegration $ mkSum trainExpr
		evalScalars scalars expr = case expr of
			Const _ c -> c
			Input _ _ -> error $ "input in eval (must be under sum somewhere)"
			Param _ _ -> Map.findWithDefault (error $ "sum "++show expr++ " is not in scalars!") expr scalars
			Un _ Sum _ -> Map.findWithDefault (error $ "sum "++show expr++ " is not in scalars!") expr scalars
			Un _ op x -> case op of
				Log -> log y
				Exp -> exp y
				Sqrt -> sqrt y
				Negate -> negate y
				where
					y = evalScalars scalars x
			Bin _ op a b -> case op of
				Plus -> x + y
				Minus -> x - y
				Mul -> x * y
				Div -> x / y
				where
					x = evalScalars scalars a
					y = evalScalars scalars b
		evalList scalars list = map (evalScalars scalars) list
		solve a b c
			| b >= 0 = Left "going up"
			| abs a < 1e-30 = Left "not quadratic"
			| otherwise = Right $ abs $ b / (2*a)
		computeScalarsLoop scalarsComputed [] = do
			let	evt@[ec, _, eb, _, ea] = evalList scalarsComputed trainExprCoefsList
			putStrLn $ "train expression evaluated: "++show evt++ " (second and fourth elements must be zero)"
			let	time2 = solve ea eb ec
			case time2 of
				Left err -> do
					putStrLn $ "can't get t^2 ("++err++"), returning failure."
					return Nothing
				Right time2 -> do
					let	time = sqrt time2 / 10
						ts = 1 : map (*time) ts
					putStrLn $ "time squared is "++show time2
					putStrLn $ "time is "++show time++ " (sqrt time / 10)"
					let	eval l = sum $ zipWith (*) ts (evalList scalarsComputed l)
						predictedOpt = eval trainExprCoefsList
					putStrLn $ "predicted train expression value "++show predictedOpt
					return $ Just $ Map.map eval paramsCoefsLists
		computeScalarsLoop scalarsComputed (stage:stages) = do
--			putStrLn $ "computing stage: "++show stage
--			putStrLn $ "current scalars: "++show scalarsComputed
			nextScalarsComputed <- computeScalarsStages scalarsComputed stage
			computeScalarsLoop nextScalarsComputed stages
		computeScalarsStages scalarsComputed [] = return scalarsComputed
		computeScalarsStages scalarsComputed (substage:substages) = do
			scalarsComputed <- computeScalarsSubstage scalarsComputed substage
			computeScalarsStages scalarsComputed substages
		computeScalarsSubstage scalarsComputed substage = do
			let	x = case scalarSubstage of
					Just x -> x
					Nothing -> vectorSumSubstage
			x `seq` return (Map.insert substage x scalarsComputed)
			where
				scalarSubstage = case substage of
					Un _ Sum e -> fmap (fromIntegral vectorSize *) $ evalAsScalar e
					_ -> evalAsScalar substage -- safe
				vectorSumSubstage = case substage of
					Un _ Sum e -> List.foldl'
						(\sum start -> (sum+) $ either UV.sum (error "must not be a scalar!") $ evalAsVector start e) 0 starts
					_ -> error "not a sum!"
				starts = [0, sliceSize..vectorSize-1]
				evalAsVector start e = case Map.lookup e scalarsComputed of
					Just x -> Right x
					Nothing -> case e of
						Input _ i -> Left $ getSlice i
						Param _ _ -> error "param must be in scalars"
						Const _ x -> Right x
						Bin _ op a b -> comb f (evalAsVector start a) (evalAsVector start b)
							where
								comb f (Left x) (Left y) = Left $ UV.zipWith f x y
								comb f (Right x) (Left y) = Left $ UV.map (f x) y
								comb f (Right x) (Right y) = Right $ f x y
								comb f (Left x) (Right y) = Left $ UV.map (flip f y) x
								f = case op of
									Plus -> (+)
									Minus -> (-)
									Mul -> (*)
									Div -> (/)
						Un _ op a -> either (Left . UV.map f) (Right . f) $ evalAsVector start a
							where
								f = case op of
									Exp -> exp
									Log -> log
									Sqrt -> sqrt
									Sum -> error "sum must be in scalars."
									Negate -> negate

					where
						len = min (vectorSize - start) sliceSize
						getSlice (In i) = UV.slice start len $ Map.findWithDefault (error "no input") i inputs
						getSlice (Out o) = UV.slice start len $ Map.findWithDefault (error "no output") o outputs
				evalAsScalar e = Map.lookup e scalarsComputed
					<|> case e of
						Input _ _ -> Nothing
						Const _ x -> Just x
						Param _ x -> error "param should be in scalars"
						Bin _ op a b -> f <$> evalAsScalar a <*> evalAsScalar b
							where
								f = case op of
									Plus -> (+)
									Minus -> (-)
									Mul -> (*)
									Div -> (/)
						Un _ op a -> f <$> evalAsScalar a
							where
								f = case op of
									Exp -> exp
									Log -> log
									Sqrt -> sqrt
									Sum -> error "sum must be in scalars."
									Negate -> negate

trainLoop bestAccuracy bestWeights sliceSize threads expr inputs outputs currentWeights eval = do
	putStrLn "Iteration started."
	(converged, accuracy) <- eval currentWeights
	putStrLn $ "accuracy percentage reported: "++show (100.0*accuracy)
	let	(newBestAccuracy, newBestWeights) = if accuracy > bestAccuracy then (accuracy, currentWeights) else (bestAccuracy, bestWeights)
	if converged
		then do
			putStrLn $ "convergence repoted"
			return newBestWeights
		else do
			newWeights <- trainStep sliceSize threads expr inputs outputs currentWeights
			case newWeights of
				Just w -> trainLoop newBestAccuracy newBestWeights sliceSize threads expr inputs outputs w eval
				Nothing -> do
					putStrLn "unable to step to new weights, returning best found."
					return newBestWeights

train sliceSize threads expr inputs outputs startWeights eval =
	trainLoop 0.0 startWeights sliceSize threads (mkSum expr) inputs outputs startWeights eval

--------------------------------------------------------------------------------
-- Simple least squares.

simpleLSExpr :: E () ()
simpleLSExpr = ofs
	where
		ofs = mkParam ()

simpleLSTrainExpr = d*d
	where
		--d = changeInput In simpleLSExpr - mkInput (Out ())
		d = changeInput In simpleLSExpr - 1 :: E (InOut () ()) ()

simpleLSTrainData :: (Map.Map () (UV.Vector Double), Map.Map () (UV.Vector Double))
simpleLSTrainData@(simpleLSInputs, simpleLSOutputs) = (Map.empty, Map.singleton () outputs)
	where
		outputs = UV.replicate 100 2

tls = train 100 2 (0.01 * simpleLSTrainExpr) simpleLSInputs simpleLSOutputs (Map.fromList [((), 0)]) check
	where
		check params = do
			putStrLn $ "param learned: "++show p
			return (abs (p-1) < 0.1, max (p-1) 0)
			where
				p = Map.findWithDefault (error "?????") () params

t = tls

--------------------------------------------------------------------------------
-- 1D perceptron test.

perceptron1Expr :: E () Bool
perceptron1Expr = softSign (alpha * (inp - ofs))
	where
		inp = mkInput ()
		ofs = mkParam False
		alpha = mkParam True

perceptron1TrainExpr = d*d
	where
		e = changeInput In perceptron1Expr
		o = mkInput (Out ())
		d = e - o

pececptron1TrainData :: (Map.Map () (UV.Vector Double), Map.Map () (UV.Vector Double))
pececptron1TrainData@(perceptron1Inputs, perceptron1Outputs) = (Map.singleton () inputs, Map.singleton () outputs)
	where
		baseOfs = 0.1
		correctIOs = [(x, if x >= baseOfs then 1 else -1) | i <- [0..1000], let x = fromIntegral (mod i 30)/15 - 1]
		noiseIOs = [ ((x-baseOfs)*0.05+baseOfs, negate v) | (x,v) <- take 300 correctIOs]
		ios = correctIOs ++ noiseIOs
		inputs = UV.fromList $ map fst ios
		outputs = UV.fromList $ map snd ios

tp1 = train 100 2 perceptron1TrainExpr perceptron1Inputs perceptron1Outputs (Map.fromList [(False, 0), (True, 1)]) (const $ return (False, 1.0))

--------------------------------------------------------------------------------
-- 2D perceptron test.

type Perc2Param = Either Bool ()

perceptron2Expr :: E Bool Perc2Param
perceptron2Expr = softSign (inA * weightA + inB * weightB + bias)
	where
		inA = mkInput False
		inB = mkInput True
		weightA = mkParam (Left False)
		weightB = mkParam (Left True)
		bias = mkParam (Right ())

perceptron2TrainExpr :: E (InOut Bool ()) Perc2Param
perceptron2TrainExpr = d*d
	where
		e = changeInput In perceptron2Expr
		o = mkInput (Out ())
		d = e - o

main = do
	r <- t
	print r
