
module Data.Introhs.Practice.Classic (module Data.Introhs.Practice.Classic) where

factR, factI :: (Integral a) => a -> a
factR n
    | n < 2 = 1
    | otherwise = n * factR (n - 1)

factI n =
    let iter prod ct
            | ct == 0 = prod
            | otherwise = iter (prod * ct) (ct - 1)
    in iter 1 n

exptR, exptI :: Float -> Float -> Float
exptR b n
    | n == 0 = 1
    | otherwise = b * exptR b (n - 1)

exptI b n = 
    let iter prod ct
            | ct == 0 = prod
            | otherwise = iter (prod * b) (ct - 1)
    in iter 1 n


libmain :: IO ()
libmain = do
    putStrLn $ "factI 5: " ++ (show $ factI 5)
