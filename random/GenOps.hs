module GenOps where

import F hiding (main)

nest :: [[String]] -> [String]
nest = map ('\t':) . concat

brk x@(Param _ _) = sh x
brk x@(Const _ _) = sh x
brk x = "(" ++ sh x ++ ")"
sh (Param _ s) = s
sh (Const _ c) = show c
sh (Bin _ op a b) = unwords [brk a, ops, brk b]
	where
		ops = case op of
			Plus -> "+"
			Minus -> "-"
			Mul -> "*"
			Div -> "/"
sh (Un _ op a) = unwords [ops, brk a]
	where
		ops = case op of
			Negate -> "negate"
			Exp -> "exp"
			Log -> "log"
			Sqrt -> "sqrt"
			Sum -> error "no support of sum!"

genBin op c1 b1 a1 c2 b2 a2 = unwords ["CBA", brk cr, brk br, brk ar]
	where
		S cr (S br (S ar _)) = op s1 s2
		s1, s2 :: S (E () String)
		s1 = S (mkParam c1) $ S (mkParam b1) $ S (mkParam a1) $ error "c1b1a1!!!"
		s2 = S (mkParam c2) $ S (mkParam b2) $ S (mkParam a2) $ error "c2b2a2!!!"

genUn op c1 b1 a1 = unwords ["CBA", brk cr, brk br, brk ar]
	where
		S cr (S br (S ar _)) = op s1
		s1 :: S (E () String)
		s1 = S (mkParam c1) $ S (mkParam b1) $ S (mkParam a1) $ error "c1b1a1!!!"

main = do
	let o x = [x]
	writeFile "Ops.hs" $ unlines $ concat $
		[ o "-- autogenerated file, see GenOps.hs"
		, o ""
		, o "{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}"
		, o ""
		, o "module Ops where"
		, o ""
		, o "import Control.Monad"
		, o ""
		, o "import qualified Data.Vector.Generic as GV"
		, o "import qualified Data.Vector.Generic.Mutable as MV"
		, o "import qualified Data.Vector.Primitive as P"
		, o "import qualified Data.Vector.Unboxed as UV"
		, o ""
		, o "-- a triple representing polynomial"
		, o "data CBA a = CBA { cbaC, cbaB, cbaA :: !a } deriving (Eq, Ord, Show)"
		, o ""
		, o "-- typical Num class instance. We do not define non-linear functions here."
		, o "instance Num a => Num (CBA a) where"
		, nest
			[ o "fromInteger x = CBA (fromInteger x) 0 0"
			, o ""
			, o "CBA c b a + CBA z y x = CBA (c+z) (b+y) (a+x)"
			, o "CBA c b a - CBA z y x = CBA (c-z) (b-y) (a-x)"
			, o "negate (CBA c b a) = CBA (negate c) (negate b) (negate a)"
			, o "abs _ = error \"no abs for CBA\""
			, o "signum _ = error \"no signum for CBA\""
			, o $ "CBA c b a * CBA cc bb aa = "++genBin (*) "c" "b" "a" "cc" "bb" "aa"
			]
		, o ""
		, o "-- Fractional class instance."
		, o "instance Fractional a => Fractional (CBA a) where"
		, nest
			[ o "fromRational x = CBA (fromRational x) 0 0"
			, o $ "CBA c b a / CBA cc bb aa = "++genBin (/) "c" "b" "a" "cc" "bb" "aa"
			]
		, o ""
		, o "-- and straightforward Floating class instance."
		, o "instance Floating a => Floating (CBA a) where"
		, nest
			[ o "pi = CBA pi 0 0"
			, o $ "exp (CBA c b a) = " ++ genUn exp "c" "b" "a"
			, o $ "log (CBA c b a) = " ++ genUn log "c" "b" "a"
			--, o $ "sqrt (CBA c b a) = " ++ genUn sqrt "c" "b" "a"
			, o "sin = error \"No sin(CBA)\""
			, o "cos = error \"No cos(CBA)\""
			, o "asin = error \"No asin(CBA)\""
			, o "acos = error \"No acos(CBA)\""
			, o "atan = error \"No atan(CBA)\""
			, o "sinh = error \"No sinh(CBA)\""
			, o "cosh = error \"No cosh(CBA)\""
			, o "asinh = error \"No asinh(CBA)\""
			, o "acosh = error \"No acosh(CBA)\""
			, o "atanh = error \"No atanh(CBA)\""
			]
		, o ""
		, o "-- Unboxed vectors's support."
		, o "newtype instance UV.MVector s (CBA a) = MV_CBA (UV.MVector s (a,a,a))"
		, o "newtype instance UV.Vector    (CBA a) = V_CBA  (UV.Vector    (a,a,a))"
		, o ""
		, o "instance (RealFloat a, UV.Unbox a) => MV.MVector UV.MVector (CBA a) where"
		, nest
			[ o "{-# INLINE basicLength #-}"
		 	, o "basicLength (MV_CBA v) = MV.basicLength v"
			, o "{-# INLINE basicUnsafeSlice #-}"
			, o "basicUnsafeSlice i n (MV_CBA v) = MV_CBA $ MV.basicUnsafeSlice i n v"
			, o "{-# INLINE basicOverlaps #-}"
			, o "basicOverlaps (MV_CBA v1) (MV_CBA v2) = MV.basicOverlaps v1 v2"
			, o "basicUnsafeCopy (MV_CBA v1) (MV_CBA v2) = MV.basicUnsafeCopy v1 v2"
			, o "basicUnsafeMove (MV_CBA v1) (MV_CBA v2) = MV.basicUnsafeMove v1 v2"
			, o "basicUnsafeGrow (MV_CBA v) n = MV_CBA `liftM` MV.basicUnsafeGrow v n"
			, o "basicUnsafeNew n = MV_CBA `liftM` MV.basicUnsafeNew n"
			, o "basicInitialize (MV_CBA v) = MV.basicInitialize v"
			, o "basicUnsafeRead (MV_CBA v) i = fmap (\\(c,b,a) -> CBA c b a) $ MV.basicUnsafeRead v i"
			, o "basicUnsafeWrite (MV_CBA v) i (CBA c b a) = MV.basicUnsafeWrite v i (c,b,a)"
			]
		, o ""
		, o "instance (RealFloat a, UV.Unbox a) => GV.Vector UV.Vector (CBA a) where"
		, nest
			[ o "{-# INLINE basicLength #-}"
			, o "{-# INLINE basicUnsafeFreeze #-}"
			, o "{-# INLINE basicUnsafeThaw #-}"
			, o "--{-# INLINE basicUnsafeSlice #-}"
			, o "--{-# INLINE basicUnsafeIndexM #-}"
			, o "basicLength (V_CBA v) = GV.basicLength v"
			, o "basicUnsafeFreeze (MV_CBA v) = V_CBA `liftM` GV.basicUnsafeFreeze v"
			, o "basicUnsafeThaw (V_CBA v) = MV_CBA `liftM` GV.basicUnsafeThaw v"
			, o "basicUnsafeSlice i n (V_CBA v) = V_CBA $ GV.basicUnsafeSlice i n v"
			, o "basicUnsafeIndexM (V_CBA cbav) i = do"
			, nest  [ o "(c, b, a) <- GV.basicUnsafeIndexM cbav i"
				, o "return $ CBA c b a"
				]
			]
		, o ""
		, o "instance (RealFloat a, UV.Unbox a) => UV.Unbox (CBA a)"
		, o ""
		, o "-- that's all!"
		]