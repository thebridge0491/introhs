
module Data.Introhs.Practice.Sequenceops (module Data.Introhs.Practice.Sequenceops) where

findIndexR, findIndexI :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexR _ _ [] = Nothing
findIndexR ndx pred1 (x:xs)
    | pred1 x = Just ndx
    | otherwise = findIndexR (ndx + 1) pred1 xs

findIndexI ndx pred1 xs = iter ndx xs
    where iter _ [] = Nothing
          iter idx (y:ys)
                | pred1 y = Just idx
                | otherwise = iter (idx + 1) ys

reverseR, reverseI :: [a] -> [a]
reverseR lst = case lst of
    [] -> []
    x:xs -> reverseR xs ++ [x]

reverseI lst = 
    let iter rst acc = case rst of
            [] -> acc
            x:xs -> iter xs (x : acc)
    in iter lst []


libmain :: IO ()
libmain = do
    putStrLn $ "reverseI " ++ (show [0,1,2,3]) ++ ": " ++ (show $ reverseI [0,1,2,3])
