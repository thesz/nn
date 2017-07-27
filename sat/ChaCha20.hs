-- |ChaCha20.hs
--
-- Some parts of ChaCha20 spec for random number generation, etc.
-- The adherence to spec is so-so, just to have something working.
--
-- Copyright (C) Serguey Zefirov 2017

module ChaCha20 where

import Control.Monad
import Control.Monad.Identity

import Data.Bits
import Data.Word

type Vec4 = (Word32, Word32, Word32, Word32)

chaChaQR :: Vec4 -> Identity Vec4
chaChaQR (a,b,c,d) = do
        (a,b,d) <- up a b d 16
        (c,d,b) <- up c d b 12
        (a,b,d) <- up a b d  8
        (c,d,b) <- up c d b  7
        return (a,b,c,d)
        where
                up a b c r = do
                        a <- return $ a + b
                        x <- return $ xor c a
                        c <- return $ rotateL x r
                        return (a,b,c)

type Vec16 = (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)

chaChaDR :: Vec16 -> Identity Vec16
chaChaDR (x00,x01,x02,x03,x04,x05,x06,x07,x08,x09,x10,x11,x12,x13,x14,x15) = do
	(x00, x04, x08, x12) <- chaChaQR (x00,x04,x08,x12)
	(x01, x05, x09, x13) <- chaChaQR (x01,x05,x09,x13)
	(x02, x06, x10, x14) <- chaChaQR (x02,x06,x10,x14)
	(x03, x07, x11, x15) <- chaChaQR (x03,x07,x11,x15)

	(x00, x05, x10, x15) <- chaChaQR (x00,x05,x10,x15)
	(x01, x06, x11, x12) <- chaChaQR (x01,x06,x11,x12)
	(x02, x07, x08, x13) <- chaChaQR (x00,x07,x08,x13)
	(x03, x04, x09, x14) <- chaChaQR (x01,x04,x09,x14)
	return (x00,x01,x02,x03,x04,x05,x06,x07,x08,x09,x10,x11,x12,x13,x14,x15)

chaChaFinal :: Vec16 -> Vec16 -> Vec16
chaChaFinal (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,aa,ab,ac,ad,ae,af) (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,ba,bb,bc,bd,be,bf) =
	(xor a0 b0, xor a1 b1, xor a2 b2, xor a3 b3, xor a4 b4, xor a5 b5, xor a6 b6, xor a7 b7
	, xor a8 b8, xor a9 b9, xor aa ba, xor ab bb, xor ac bc, xor ad bd, xor ae be, xor af bf
	)

chaCha08 :: Vec16 -> Identity Vec16
chaCha08 v0 = do
	v2 <- chaChaDR v0
	v4 <- chaChaDR v2
	v6 <- chaChaDR v4
	v8 <- chaChaDR v6
	return $ chaChaFinal v8 v0

randoms :: [Word32]
randoms = undefined
