{-# LANGUAGE FlexibleContexts #-}
module Data.Introhs.Util (module Data.Introhs.Util) where

import qualified System.IO as IO

import qualified System.Log as Log
import qualified System.Log.Handler as Handler
import qualified System.Log.Handler.Simple as Handler.Simple
import qualified System.Log.Formatter as Formatter

import qualified Text.Regex.Posix as RegexP

--import qualified Data.ConfigFile as ConfigFile
import qualified Data.Ini as Ini

withFormatter :: Handler.LogHandler a => a -> a
withFormatter hdlr = Handler.setFormatter hdlr fmtr
  --http://hackage.haskell.org/packages/archive/hslogger/1.1.4/doc/html/System-Log-Formatter.html
  where fmtr = Formatter.simpleLogFormatter "$time [$loggername $prio]: $msg"

logStreamHdlrWithFormat :: (Handler.Simple.GenericHandler IO.Handle -> b) -> IO.Handle -> Log.Priority -> IO b
logStreamHdlrWithFormat fmtr stream logpri = do
    hdlr' <- Handler.Simple.streamHandler stream logpri
    return $ fmtr hdlr'

logFileHdlrWithFormat :: (Handler.Simple.GenericHandler IO.Handle -> b) -> FilePath -> Log.Priority -> IO b
logFileHdlrWithFormat fmtr path logpri = do
    hdlr' <- Handler.Simple.fileHandler path logpri
    return $ fmtr hdlr'

-- need to use FlexibleContexts
(=~+) :: (RegexP.RegexMaker RegexP.Regex RegexP.CompOption RegexP.ExecOption pat, RegexP.RegexContext RegexP.Regex src tgt) => src -> pat -> tgt
(=~+) src pat = RegexP.match rexp src
  where rexp = RegexP.makeRegexOpts (RegexP.defaultCompOpt +
          RegexP.compIgnoreCase) RegexP.defaultExecOpt pat

{-
iniCfgToString :: ConfigFile.ConfigParser -> String
iniCfgToString cfg = foldr (\el acc1 -> foldr (\(a, b) acc2 -> 
    concat ["(", a, ", ", b, ")", ", ", acc2]) "" el ++ acc1) "" items
  where items = foldl (\acc el -> 
            let optVals = either (const []) id $ ConfigFile.items cfg el
            in map (\(o, v) -> (concat [el, ":", o], v)) optVals : acc) [] $
                "DEFAULT" : ConfigFile.sections cfg
-}
{-
iniCfgToString :: ConfigFile.ConfigParser -> String
iniCfgToString cfg = ConfigFile.to_string cfg
-}
iniCfgToString :: Ini.Ini -> String
iniCfgToString cfg = show $ Ini.unIni cfg

mkStringInit :: (String, String, String) -> (a -> String) -> [a] -> String
mkStringInit (beg, sep, stop) el_fmt lst = case lst of
    [] -> beg ++ stop
    _ -> beg ++ foldr (\el acc -> el_fmt el ++ 
        (if "" == acc then "" else sep) ++ acc) "" lst ++ stop

mkString :: (a -> String) -> [a] -> String
mkString = mkStringInit ("[", ", ", "]")

mkStringNested :: (String, String, String) -> (a -> String) -> [[a]] -> String
mkStringNested (beg, sep, stop) el_fmt nlsts = case nlsts of
    [] -> beg ++ stop
    _ -> foldl (\acc el -> acc ++ mkStringInit ("", sep, "\n") el_fmt el)
        beg nlsts ++ stop

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f3 (x, y, z) = f3 x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f3 x y z = f3 (x, y, z)

headOr :: t -> [t] -> t
headOr nullDefault xs = case xs of
    [] -> nullDefault
    x:_ -> x

inEpsilon :: (Ord a, Num a) => a -> a -> a -> Bool
inEpsilon tolerance a b =
    --(a - delta) <= b && (a + delta) >= b
    not ((a + delta) < b) && not ((b + delta) < a)
  where delta = abs tolerance

cartesianProd :: [a] -> [b] -> [(a, b)]
cartesianProd xs ys =
{-  do
        x <- xs
        y <- ys
        if True
        then [(x, y)]
        else [] -}
    {- concatMap (\x -> filter (\e -> True) (map (\y -> (x, y)) ys)) xs -}
    [(x, y) | x <- xs, y <- ys, True]


libmain :: IO ()
libmain = do
    let res = [(x, y) | x <- [0, 1, 2], y <- [10, 20, 30], True]
    putStrLn $ "cartesianProd [0, 1, 2] [10, 20, 30]: " ++
        mkString (\(t0, t1) -> "(" ++ (show t0) ++ ", " ++ 
        (show t1) ++ ")") res
