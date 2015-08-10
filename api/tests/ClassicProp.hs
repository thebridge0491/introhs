{-# LANGUAGE ScopedTypeVariables #-}
module ClassicProp (tprops) where

import Data.List
import Data.Word
import qualified Control.Monad.Writer as Writer

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
--import Test.QuickCheck.Property as Prop

import qualified Data.Introhs.Util as Util
import Data.Introhs.Practice.Classic

epsilon = 0.001

propSquare :: Int -> Property
propSquare n = (n >= 1) ==>
    foldl (\a f -> a && Util.inEpsilon (epsilon * ans) ans (f $ realToFrac n)) True
    [squareR, squareI, squareF, squareU, squareLc]
  where ans = realToFrac n ** 2.0

--propExpt :: (Eq a, Show a, Num a) => a -> a -> Bool
propExpt :: Float -> Float -> Bool
propExpt b n = foldl (\a f -> a && (Util.inEpsilon (epsilon * ans) ans (f b n))) 
    True [exptR, exptI, exptF, exptU, exptLc, fastExptR, fastExptI]
  where ans = b ** n

propSumTo :: Int -> Int -> Bool
propSumTo hi lo = foldl (\a f -> a && f hi lo == ans) True
    [sumToR, sumToI, sumToF, sumToU, sumToLc]
  where ans = foldl (+) lo [(lo + 1)..hi]

--propFact :: (Eq a, Show a, Ord a, Num a, Enum a) => a -> Bool
propFact :: Word32 -> Bool
propFact n = foldl (\a f -> a && f n == ans) True [factR, factI, factF
	, factU, factLc]
  where ans = foldl (*) 1 [1..n]

propFib :: Int -> Bool
propFib n = foldl (\a f -> a && f n == ans) True 
    [fibR, fibI, fibF, fibU, fibLc] 
  where ans = snd $ foldl (\(s0, s1) _ -> (s0 + s1, s0)) (0, 1) [0..n]

propPascaltri :: Int -> Bool
propPascaltri rows = foldl (\a f -> a && validNumRows (f rows) &&  
        fst (foldl (\(a1, n) r -> (a1 && validLenRow n r && validSumRow n r, 
        n + 1)) (True, 0) (f rows))) True
        [pascaltriMult, pascaltriAdd, pascaltriF, pascaltriU, pascaltriLc]
  where validNumRows res = length res == rows + 1
        validLenRow n r = length r == n + 1
        validSumRow n r = sum r == truncate (2 ** realToFrac n)

propQuotRem :: Int -> Int -> Property
propQuotRem m n = (n /= 0) ==> foldl (\a (fQ, fR) -> a && fQ m n == ansQ 
    && fR m n == ansR) True
    [(quotM, remM)]
  where (ansQ, ansR) = quotRem m n

propDivMod :: Int -> Int -> Property
propDivMod m n = (n /= 0) ==> foldl (\a (fD, fM) -> a && fD m n == ansD
    && fM m n == ansM) True
    [(divM, modM)]
  where (ansD, ansM) = divMod m n

propGcdLcm :: [Int] -> Property
propGcdLcm xss = case xss of
    x:xs -> (length (x:xs) < 20) ==> 
        foldl (\a (fG, fL) -> a && fG (x:xs) == ansG && fL (x:xs) == ansL
        ) True
        [(gcdR, lcmR), (gcdI, lcmI), (gcdF, lcmF), (gcdU, lcmU)]
      where (ansG, ansL) = (foldl gcd (abs x) xs, foldl lcm (abs x) xs)
    _ -> True ==> True

propBaseExpand :: Int -> Int -> Bool
propBaseExpand b n = foldl (\a f -> a && f b n == ans) True 
    [baseExpandR, baseExpandI, baseExpandF, baseExpandU, baseExpandLc]
  where ans = reverse $ unfoldr (\x -> if x == 0 then Nothing
            else Just (mod x b, div x b)) n

propBaseTo10 :: Int -> [Int] -> Bool
propBaseTo10 b lst = foldl (\a f -> a && f b lst == ans) True 
    [baseTo10R, baseTo10I, baseTo10F, baseTo10U, baseTo10Lc]
  where ans = snd $ foldr (\e (h, t) -> 
            (h + 1, t + (e * truncate (fromIntegral b ** fromIntegral h))))
            (0, 0) lst

propRange :: Int -> Int -> Bool
propRange hi lo = foldl (\a (f, fStep) -> a && f lo hi == ans 
    && fStep 1 lo hi == ans) True
    [(rangeR, rangeStepR), (rangeI, rangeStepI), (rangeF, rangeStepF)
		, (rangeU, rangeStepU)]
  where ans = [lo..hi]

propComposeInt :: (Eq c, Show c, Integral c) => (b -> c) -> (a -> b) -> a -> Bool
propComposeInt f g x = foldl (\a comp -> a && ans == comp f g x)
    True [compose1]
  where ans = (f . g) x

propComposeFlt :: (Eq c, Show c, Ord c, Floating c) => (b -> c) -> (a -> b) -> a -> Bool
propComposeFlt f g x = 
    foldl (\a comp -> a && Util.inEpsilon (realToFrac epsilon * ans) ans (comp f g x))
    True [compose1]
  where ans = (f . g) x


--propFactLog :: (Eq a, Show a, Num a) => Integer -> Bool
propFactLog :: Integer -> Bool
propFactLog n = foldl (\a f -> do
    let (numFact, logFactI) = Writer.runWriter $ f n
    a && numFact == ans && foldl (\a1 (_, msg) -> a1 && not (null msg))
        True logFactI) True
    [factILog]
  where ans = foldl (*) 1 [1..(toInteger n)]

    
-- Data.Functor.<$> ; GHC.Base.<*>   -- prefix fmap --> infix <$>
genTup2Int = (,) <$> (elements ([1..20] :: [Float])) <*>
    (elements ([2..10] :: [Float]))
genTup2 xs ys = (,) <$> elements xs <*> elements ys

--propBase2Expand = propBaseExpand 2
--propBase11Expand = propBaseExpand 11

propBase2To10 lst = (length lst < 20) ==> propBaseTo10 2 lst
propBase16To10 lst = (length lst < 10) ==> propBaseTo10 16 lst

propSquareSqrt = propComposeFlt (** 2.0) sqrt
propLengthRange = propComposeInt length (\n -> [0..n])

--propComb1 = forAll (genTup2 ([1..20] :: [Float]) ([2..10] :: [Float])) . uncurry


tprops = map (uncurry testProperty) $ 
    [("square n == n ** 2.0", property propSquare)
    , ("expt b n == b ** n", (forAll genTup2Int . uncurry) propExpt)
    , ("sumTo hi lo == List.sum [lo..hi]", property propSumTo)
    , ("fact n == List.product [1..n]", forAll (elements [0..18]) propFact)
    , ("fib n == ?? n", forAll (elements [0..20]) propFib)
    , ("pascaltri rows == ?? rows", forAll (elements [0..15]) propPascaltri)
    , ("(quotM m n, remM m n) == quotRem m n", property propQuotRem)
    , ("(divM m n, modM m n) == divMod m n", property propDivMod)
    , ("[gcd | lcm] m n == [gcd | lcm] m n"
        , forAll (listOf1 $ elements [(-500)..500]) propGcdLcm)
    , ("base2Expand n == ?? 2 n"
        , forAll (elements [0..150]) (propBaseExpand 2))
    , ("base11Expand n == ?? 11 n"
        , forAll (elements [0..250]) (propBaseExpand 11))
    , ("base2To10 digits == ?? 2 digits"
        , forAll (listOf1 $ elements [0..1]) propBase2To10)
    , ("base16To10 digits == ?? 16 digits"
        , forAll (listOf1 $ elements [0..15]) propBase16To10)
    , ("range hi lo == [lo..hi]", property propRange)
    , ("composeLenRange == len . range"
        , forAll (elements [0..20]) propLengthRange)
    , ("composeSquareSqrt == square . sqrt"
        , forAll (elements [0.0..100.0]) propSquareSqrt)
    , ("factLog n == List.product [1..n]"
        , forAll (elements [0..18]) propFactLog)
    ]
