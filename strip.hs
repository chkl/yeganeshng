module Main where

import Data.Map (mapKeysWith)
import System.Environment (getArgs)
import Version (versionStrip)
import Yeganesh (Options, deprecate, inFileName, merge, profile, parseOptions,
    readPossiblyNonExistent, stripNewline, writeProfile)

runWithOptions :: Options -> IO ()
runWithOptions opts = do
    inFile      <- inFileName (profile opts)
    (t, cmd)    <- readPossiblyNonExistent inFile
    writeProfile opts (t, mapKeysWith merge stripNewline cmd)
    deprecate inFile (profile opts)

introText :: String
introText = unlines $ [
    versionStrip,
    "Usage: yeganesh-strip [-p profile]",
    "The other options described below are accepted, but ignored.",
    "Profiles are stored in the XDG data home for yeganesh."]

main :: IO ()
main = getArgs >>= either putStr runWithOptions . parseOptions introText versionStrip
