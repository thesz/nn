-- |ChaCha20.hs
--
-- Some parts of ChaCha20 spec for random number generation, etc.
--
-- Copyright (C) Serguey Zefirov 2017

module ChaCha20 where

import Data.Bits
import Data.Word

chaChaQR :: (Word64, Word64, Word64, Word64) -> GenM Vec4
chaChaQR point (a,b,c,d) = do
        (a,b,d) <- up a b d 16
        (c,d,b) <- up c d b 12
        (a,b,d) <- up a b d  8
        (c,d,b) <- up c d b  7
        return (a,b,c,d)
        where
                p s a b c d = do
--                        commentVec ("QR_"++point++"_"++s++"_a") a
--                        commentVec ("QR_"++point++"_"++s++"_b") b
--                        commentVec ("QR_"++point++"_"++s++"_c") c
--                        commentVec ("QR_"++point++"_"++s++"_d") d
                        return ()
                up a b c r = do
                        a <- genAdd a b
                        x <- genXor c a
                        c <- genRot x r
                        return (a,b,c)
