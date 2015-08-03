module Data.Introhs.Intro (module Data.Introhs.Intro) where

import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Trans as Monad.Trans
import qualified Control.Exception as Exception
import qualified Control.Concurrent as Concurrent

import qualified System.IO as IO
import qualified System.Log as Log

{- | Compose greeting -}
-- ^ greetPath - File path to greet text prefix
-- ^ name - String name
greetingIO :: FilePath -> String -> IO String
greetingIO greetPath name = do
    lineEither <- Exception.try $ IO.withFile greetPath IO.ReadMode
        IO.hGetLine
    return $ case lineEither of
        Left exc -> Exception.throw (exc :: Exception.IOException)
        Right line -> line ++ name ++ "!"

{- | Compose greeting with Writer to log -}
-- ^ greetPath - File path to greet text prefix
-- ^ name - String name
greetingLog :: FilePath -> String -> Writer.WriterT [(Log.Priority, String)]
    IO String
greetingLog greetPath name = do
    Writer.tell [(Log.INFO, "greeting()")]
    Monad.Trans.liftIO $ greetingIO greetPath name

delayCharR :: (() -> IO a) -> IO Char
delayCharR delayFunc = do
    _ <- delayFunc ()
    putStr "Type any character when ready."
    IO.hFlush IO.stdout
    ch <- getChar
    if '\n' /= ch && '\0' /= ch then return ch
    else delayCharR delayFunc


libmain :: IO ()
libmain = do
    putStrLn $ "delayCharR (\\_ -> Concurrent.threadDelay $ 2 * 10 ^ 6)"
    _ <- delayCharR (\_ -> Concurrent.threadDelay $ 2 * 10 ^ 6)
    putStrLn ""
