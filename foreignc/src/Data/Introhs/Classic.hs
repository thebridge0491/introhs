-- {-# INCLUDE "intro_c/classic.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Introhs.Classic (factI, factLp, exptI, exptLp) where

import Data.Word
import Foreign.C

foreign import ccall "fact_i" _fact_i :: CUInt -> Word64
foreign import ccall "fact_lp" _fact_lp :: CUInt -> Word64

foreign import ccall "expt_i" _expt_i :: CFloat -> CFloat -> Float
foreign import ccall "expt_lp" _expt_lp :: CFloat -> CFloat -> Float

factI, factLp :: Word64 -> Word64
factI n = _fact_i (fromIntegral n :: CUInt)
factLp n = _fact_lp (fromIntegral n :: CUInt)

exptI, exptLp :: Float -> Float -> Float
exptI b n = _expt_i (realToFrac b :: CFloat) (realToFrac n :: CFloat)
exptLp b n = _expt_lp (realToFrac b :: CFloat) (realToFrac n :: CFloat)


libmain :: IO ()
libmain = do
    putStrLn $ "factI 5: " ++ (show $ factI 5)
