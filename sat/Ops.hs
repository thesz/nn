-- |Ops.hs
--
-- Operations for whatever was defined in Types.hs.
--
-- Copyright (C) Serguey Zefirov 2017

module Ops (
	  module Ops
	, module Types
	) where

import Data.Bits

import Types

userToL :: Int -> L
userToL i = (if i < 0 then lnot else id) $ L (fromIntegral $ abs i `shiftL` 1)

lnot :: L -> L
lnot = L . xor 1 . litIndex

