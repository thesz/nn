-- |I.hs
--
-- Interval arithmetic - very simple implementation.
--
-- Copyright (C) 2017 Serguey Zefirov.

module I where

data I a = I !a !a
	deriving (Eq, Ord, Show)

instance (Ord a, Num a) => Num (I a) where
	fromInteger a = let x = fromInteger a in I x x
	I la ha + I lb hb = I (la + lb) (ha + hb)
	I la ha - I lb hb = I (la - hb) (ha - lb)
	I la ha * I lb hb
		-- important special case:
		| la >= 0 && lb >= 0 = I x1 x4
		| otherwise = I (min (min x1 x2) (min x3 x4)) (max (max x1 x2) (max x3 x4))
		where
			x1 = la * lb
			x2 = la * hb
			x3 = ha * lb
			x4 = ha * hb
	abs x@(I l h)
		| l >= 0 = x
		| h <= 0 = I (abs h) (abs l)
		| otherwise = I 0 (max (abs l) (abs h))
	signum (I l h)
		| l > 0 = 1
		| h < 0 = -1
		| l == 0 && h == 0 = 0
		| otherwise = I (-1) 1
	negate (I l h) = I (negate h) (negate l)

instance (Ord a, Fractional a) => Fractional (I a) where
	fromRational r = let x = fromRational r in I x x
	a / b@(I lb hb)
		| lb <= 0 && hb >= 0 = error $ "range contains zero."
		| otherwise = a * recip b
	recip (I l h)
		| l <= 0 && h >= 0 = error $ "range contains zero."
		| otherwise = I (1/h) (1/l)

instance (Ord a, Floating a) => Floating (I a) where
	pi = I pi pi
	exp (I a b) = I (exp a) (exp b)
	log = error "log (I) is undefined"
	sin = error "sin (I) is undefined"
	cos = error "cos (I) is undefined"
	asin = error "asin (I) is undefined"
	acos = error "acos (I) is undefined"
	atan = error "atan (I) is undefined"
	sinh x = (exp x - exp (negate x))/ 2
	cosh x = (exp x + exp (negate x))/ 2
	asinh = error "asinh (I) is undefined"
	acosh = error "acosh (I) is undefined"
	atanh = error "atanh (I) is undefined"
