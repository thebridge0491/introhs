#!/bin/sh
{-
EXTRA_PKGDB=`find $HOME/.stack/snapshots -type f -name package.cache -exec dirname {} \;`
DEPNS_PKGIDS=`for pkg in base hslogger ConfigFile regex-compat random aeson json yaml ; do echo -n -package-id= ; ghc-pkg --global --user --package-db=$EXTRA_PKGDB --simple-output field $pkg id | head -n1 ; done`

#exec ghci -v1 -Wall -rtsopts -with-rtsopts=-N -i -isrc -global-package-db -user-package-db -package-db=$EXTRA_PKGDB $DEPNS_PKGIDS $0
exec runhaskell -v1 -Wall -rtsopts -with-rtsopts=-N -i -isrc -global-package-db -user-package-db -package-db=$EXTRA_PKGDB $DEPNS_PKGIDS $0 $@
-}
-- #!/usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Data.Maybe as Maybe

import qualified System.Log as Log
import qualified System.Log.Handler as Handler
import qualified System.Log.Logger as Logger
import qualified Control.Monad as Monad
import qualified Control.Monad.Writer as Writer
import qualified Control.Concurrent as Concurrent
import Text.Printf
import Data.List

import qualified System.Console.GetOpt as GetOpt
import qualified Control.Exception as Exception
import qualified System.Random as Random

import qualified Text.Regex as Regex
--import qualified Text.Regex.Posix as RegexP

import qualified Data.ConfigFile as ConfigFile
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BChar8
import qualified Data.ByteString.Lazy.Char8 as BLChar8
import qualified Data.HashMap.Lazy as H
import qualified Data.Aeson as AeJSON
import qualified Data.Yaml as Yaml
import qualified Text.JSON as JSON

import qualified Data.Vector as V
import qualified Data.Time as Time
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Time.Clock.POSIX as Time.POSIX
import qualified Data.Time.LocalTime as Time.Local
import qualified Data.Time.Format as Time.Format

import qualified Data.Introhs.Util as Util
import qualified Data.Introhs.Intro as Intro
import qualified Data.Introhs.Person as Person
import qualified Data.Introhs.Practice.Classic as Classic
import qualified Data.Introhs.Practice.Sequenceops as Sequenceops

-- single-line comment
{- multi-line comment
    --- run w/out compile ---
    [sh | runhaskell -i$(pwd)] Main.hs arg1 argN
    
    --- run REPL, load script, & run ---
    ghci -i$(pwd) Main.hs ; :main arg1 argN
    
    --- help/info tools in REPL ---
    :show, :help|:?, :type, :kind, :info, :browse, :trace, :load
-}

data OptsRecord = OptsRecord {optHelp :: Bool, optUser :: String, 
        optNum :: Int, optIsExpt2 :: Bool, optVerbose :: Int}
    deriving (Eq, Show)

defaultOpts :: OptsRecord
defaultOpts = OptsRecord {optHelp = False, optUser = "World", optNum = 0,
    optIsExpt2 = False, optVerbose = 1}

data UserType = User {userName :: String, userNum :: Int, 
    userTimeIn :: Float}
    deriving (Eq, Show)

defaultUser :: UserType
defaultUser = User {userName = "NoName", userNum = 0, userTimeIn = 0.0}

(logger_root, logger_prac) = (Logger.rootLoggerName, "prac")

runIntro :: String -> OptsRecord -> IO ()
runIntro rsrc_path opts = do
    timeIn <- Time.Clock.getCurrentTime
    progName <- Environment.getProgName
    let user1 = User {userName = (optUser opts), userNum = (optNum opts), 
        userTimeIn = realToFrac (Time.POSIX.utcTimeToPOSIXSeconds 
        timeIn)}
    dt1 <- Time.Local.getZonedTime
    let dateStr = Time.Format.formatTime Time.defaultTimeLocale "%c" dt1
    rndGen <- Random.newStdGen
    
    Exception.assert (numVal == V.length numVec * numVec V.! 0)
        (putStr "")
    _ <- Intro.delayCharR (\_ -> Concurrent.threadDelay $ delaySecs * 10 ^ 6)
    
    let user2 = case userNum user1 of
            0 -> user1 {userNum = fst $ Random.randomR ((2, 20) :: 
                (Int, Int)) rndGen}
            _ -> user1
    let num1 = userNum user2
    
    --greetTxt <- Intro.greetingIO (rsrc_path ++ "/greet.txt") user
    (greetTxt, log_greeting) <- Writer.runWriterT $ Intro.greetingLog 
        (rsrc_path ++ "/greet.txt") (optUser opts)
    mapM_ (uncurry (Logger.logM logger_prac)) log_greeting
    putStrLn $ dateStr ++ "\n" ++ greetTxt
    
    putStrLn $ (if match then "Good match: " else "Does not match: ") ++
        (optUser opts) ++ " to \"^quit$\""
    
    time2 <- Time.Clock.getCurrentTime
    let timeDiff = Time.Clock.diffUTCTime time2 timeIn
    printf "(program %s) Took %.2f seconds.\n" progName
        (realToFrac timeDiff :: Float)
    putStrLn $ replicate 40 '#'
    
    if (optIsExpt2 opts) then do
        printf "expt 2 %f: %f\n" (fromIntegral num1 :: Float) $  
            Classic.exptI 2 (fromIntegral num1 :: Float)
        printf "reverse %s: %s\n" (show lst) $ show $
            Sequenceops.reverseI lst
        printf "sortBy <lambda> %s: %s\n" (show $ [9, 9, 9, 9] ++ lst) $ 
            show $ sortBy (flip compare) $ [9, 9, 9, 9] ++ lst
        else do
            printf "fact %d: %d\n" num1 $ Classic.factI num1
            printf "findIndex 0 <lambda> %s: %d\n" (show lst) $ 
                Maybe.fromMaybe (-1) $ Sequenceops.findIndexI 0 (== 3) lst
            printf "append %s %s: %s\n" (show [9, 9, 9, 9]) (show lst) $ 
                show $ [9, 9, 9, 9] ++ lst

    putStrLn $ replicate 40 '#'
    printf "Person.personAge person1: %d\n" $ Person.personAge person1
    printf "person2 = Person.Person (personName person1) 33: %s\n" $ 
        show person2
    printf "show person1: %s\n" $ show person1
    printf "show person2: %s\n" $ show person2
    putStrLn $ replicate 40 '#'
  where
    (person1, lst) = (Person.Person "I.M. Computer" 32, [2, 1, 0, 4, 3])
    (numVec, delaySecs) = (V.fromList [11, 0o13, 0xb, 11], 2)
    numVal = V.foldr (+) 0 numVec
    --match = (optUser opts) RegexP.=~ "^quit$" :: Bool
    --match = (optUser opts) Util.=~+ "^quit$" :: Bool
    (isMatchBOLEOL, isCaseSensitive) = (True, False)
    pat = Regex.mkRegexWithOpts "^quit$" isMatchBOLEOL isCaseSensitive
    match = Maybe.isJust $ Regex.matchRegex pat (optUser opts)
    person2 = Person.Person (Person.personName person1) 33


getConfigIO :: FilePath -> IO ConfigFile.ConfigParser
getConfigIO confFile = do
    --ioConfParser <- ConfigFile.readfile ConfigFile.emptyCP confFile
    --return $ either (const ConfigFile.emptyCP) id ioConfParser
    e <- Exception.try $ ConfigFile.readfile ConfigFile.emptyCP confFile
    case e of
        Left (exc :: Exception.IOException) -> 
            return ConfigFile.emptyCP
        Right ioConfParser -> return $ either (const ConfigFile.emptyCP) id ioConfParser

deserializeStr :: String -> String -> AeJSON.Object
deserializeStr str1 fmt1 = 
    if "yaml" == fmt1 then
        Maybe.fromMaybe H.empty (Yaml.decode (BChar8.pack str1) :: 
            Maybe AeJSON.Object)
        else
        Maybe.fromMaybe H.empty (AeJSON.decode (BLChar8.pack str1) :: 
            Maybe AeJSON.Object)

specList :: [GetOpt.OptDescr (OptsRecord -> OptsRecord)]
specList =
    [GetOpt.Option "h" ["help"]
        (GetOpt.NoArg (\opt -> opt {optHelp = True}))
        "Display help msg"
    , GetOpt.Option "u" ["user"]
        (GetOpt.ReqArg (\arg opt -> opt {optUser = arg}) "USER")
        "User name"
    , GetOpt.Option "n" ["num"]
        (GetOpt.ReqArg (\arg opt -> opt {optNum = read arg :: Int}) "NUM")
        "Number n"
    , GetOpt.Option "2" ["expt2"]
        (GetOpt.NoArg (\opt -> opt {optIsExpt2 = True}))
        "Expt 2 n"
    , GetOpt.Option "v" ["verbose"]
        (GetOpt.OptArg ((\v opt -> opt {optVerbose = read v :: Int}) .
            Maybe.fromMaybe "3") "VERBOSE")
        "Verbosity level (Optional) (default 1)"
    ]

parseCmdOpts :: String -> [String] -> (OptsRecord, [String])
parseCmdOpts progName argv =
    case GetOpt.getOpt GetOpt.Permute specList argv of
        (o, n, []) -> (foldl (flip id) defaultOpts o, n)
        (_, _, errs) -> error (show errs ++  "\n" ++ header)
  where header = "Usage: " ++ progName ++ " [-h][OPTION...]"

parseCmdOptsLog :: String -> [String] ->
    Writer.Writer [(Log.Priority, String)] (OptsRecord, [String])
parseCmdOptsLog progName argv = do
    Writer.tell [(Log.INFO, "parseCmdOpts()")]
    return (parseCmdOpts progName argv)

checkUserHelp :: Bool -> String -> (() -> IO a) -> IO ()
checkUserHelp isHelp progName exitFunc =
    Monad.when isHelp $ do 
        IO.hPutStrLn IO.stderr (GetOpt.usageInfo header specList)
        _ <- exitFunc ()
        Exit.exitWith Exit.ExitSuccess
  where header = "Usage: " ++ progName ++ " [-h][OPTION...]"

{- | Entry point -}
main :: IO ()
main = do
    argv <- Environment.getArgs
    progName <- Environment.getProgName
    envRsrcPath <- Environment.lookupEnv "RSRC_PATH"
    let rsrc_path = Maybe.fromMaybe "resources" envRsrcPath
    
    logStream_root <- Util.logStreamHdlrWithFormat Util.withFormatter 
        IO.stderr Logger.DEBUG
    logFile_root <- Util.logFileHdlrWithFormat Util.withFormatter 
        "root.log" Logger.INFO
    logFile_prac <- Util.logFileHdlrWithFormat Util.withFormatter 
        "prac.log" Logger.DEBUG
    Logger.updateGlobalLogger logger_root $ Logger.setLevel Logger.INFO
    Logger.updateGlobalLogger logger_root $ Logger.setHandlers [logStream_root,
        logFile_root]
    Logger.updateGlobalLogger logger_prac $ Logger.setHandlers [logFile_prac]
    
    let exitCloseLogs () = mapM_ Handler.close 
            [logStream_root, logFile_root, logFile_prac]
    
    let ((opts, _), log_parseCmdOpts) = Writer.runWriter $ 
            parseCmdOptsLog progName argv
    mapM_ (uncurry (Logger.logM logger_root)) log_parseCmdOpts
    checkUserHelp (optHelp opts) progName exitCloseLogs
    
    ini_cfg <- getConfigIO $ rsrc_path ++ "/prac.conf"
    {- --jsonstr <- readFile $ rsrc_path ++ "/prac.json"
    e1 <- Exception.try $ readFile $ rsrc_path ++ "/prac.json"
    --yamlstr <- readFile $ rsrc_path ++ "/prac.yaml"
    e2 <- Exception.try $ readFile $ rsrc_path ++ "/prac.yaml"
    let jsonstr = case e1 of
            Left (exc :: Exception.IOException) -> 
                "{\"domain\": \"???\", \"user1\": {\"name\": \"???\"}}"
            Right str -> str
    let hmap1 = if True then deserializeStr jsonstr "json"
                else deserializeStr yamlstr "yaml"
                  where yamlstr = case e2 of
                            Left (exc :: Exception.IOException) -> 
                                "domain: ???\nuser1: \n  name: ???\n"
                            Right str -> str
    -}
    {- let jsonobj = JSON.decode jsonstr :: JSON.Result (JSON.JSObject JSON.JSValue)
    let domain1 = jsonobj >>= (JSON.valFromObj "domain") :: JSON.Result JSON.JSString
    let user1Name1 = jsonobj >>= (JSON.valFromObj "user1") >>=
            (JSON.valFromObj "name") :: JSON.Result JSON.JSString
    
    let AeJSON.Object user1Lst = H.lookupDefault AeJSON.Null "user1" hmap1
    let AeJSON.String domain = H.lookupDefault "" "domain" hmap1
    let AeJSON.String user1Name = H.lookupDefault "" "name" user1Lst
    -}
    let lst = [(Util.iniCfgToString ini_cfg 
            , either (const "") id $ ConfigFile.get ini_cfg "default" "domain"
            , either (const "") id $ ConfigFile.get ini_cfg "user1" "name")
            {- , (show $ (\(JSON.Ok alst) -> alst) jsonobj
            , JSON.fromJSString ((\(JSON.Ok res) -> res) domain1)
            , JSON.fromJSString ((\(JSON.Ok res) -> res) user1Name1))
            , (show hmap1, Text.unpack domain, Text.unpack user1Name) -}
            ]
    
    mapM_ (\(t0, t1, t2) -> do
        putStr $ "config: {" ++ t0 ++ "}\n"
        putStr $ "domain: " ++ t1 ++ "\n"
        putStr $ "user1Name: " ++ t2 ++ "\n\n") lst
    
    runIntro rsrc_path opts
    
    exitCloseLogs ()
    
{-
-- System.Environment.withArgs [arg1, argN] main
-- :set args arg1 argN
:main arg1 argN
-}
