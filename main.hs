-- boilerplate {{{
{-# LANGUAGE CPP #-}
module Main where

import Catch (catch)
import Control.Arrow (second)
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar, forkIO)
import Control.Monad (filterM, forM)
import Data.Map (empty, intersection, union)
import Data.Time (getCurrentTime)
import Prelude hiding (catch)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hClose, hGetContents, hPutStr, stderr, stdout)
import System.Posix.Files (fileAccess, getFileStatus, isDirectory)
import System.Process (runInteractiveProcess, waitForProcess)
import Version (CurrentFormat, versionYeganesh)
import Yeganesh (Commands, Options, addEntries, deprecate, dmenuOpts,
    executables, inFileName, parseInput, parseOptions, parsePath, profile,
    prune, readPossiblyNonExistent, showPriority, stripNewline, updatePriority,
    writeProfile)
import qualified System.IO.Strict as Strict (getContents)
-- }}}
-- IO stuff {{{
catchList :: IO [a] -> IO [a]
catchList = flip catch (const . return $ [])
-- }}}
-- shell stuff {{{
dmenu :: [String] -> CurrentFormat -> IO (ExitCode, CurrentFormat)

#ifdef profiling
-- when profiling, it's convenient to skip the call out to dmenu
dmenu opts cv = return (ExitSuccess, cv)
#endif

dmenu opts cv@(_, cmds) = do
    (hIn, hOut, hErr, p) <- runInteractiveProcess "dmenu" opts Nothing Nothing
    hPutStr hIn (showPriority cmds)
    hClose hIn
    o <- hGetContents hOut
    e <- hGetContents hErr
    c <- waitForProcess p
    hPutStr stdout o
    hPutStr stderr e
    cv' <- updateState c o cv
    return (c, cv')

updateState :: ExitCode -> String -> CurrentFormat -> IO CurrentFormat
updateState (ExitFailure {})  _  (t, cmds) = return (t, cmds)
updateState  ExitSuccess     cmd (t, cmds) = do
    now <- getCurrentTime
    return (now, updatePriority (stripNewline cmd) t now cmds)

lsx :: Bool -> IO (IO [String])
lsx False = return (return [])
lsx True  = do
    mvar <- newEmptyMVar
    _    <- forkIO $ do
        path  <- catchList $ getEnv "PATH"
        execs <- catchList . forM (parsePath path) $ \s ->
            catchList (getDirectoryContents s) >>=
            filterM (executable . (s </>))
        putMVar mvar (concat execs)
    return (takeMVar mvar)
    where
    executable file = flip catch (\_ -> return False) $ do
        status <- getFileStatus file
        case isDirectory status of
            True  -> return False
            False -> fileAccess file True False True
            -- TODO: do people prefer to see files which are executable by
            -- *someone*, even if not by the current user? ask brisbin on
            -- Freenode, who is to date the only person to request this feature

parseStdin :: Bool -> IO Commands
parseStdin False = fmap parseInput Strict.getContents
parseStdin True  = return empty

runWithOptions :: Options -> IO ()
runWithOptions opts = do
    future          <- lsx (executables opts)
    inFile          <- inFileName (profile opts)
    cached          <- readPossiblyNonExistent inFile
    new             <- parseStdin (executables opts)
    (code, updated) <- dmenu (dmenuOpts opts) (second (`combine` new) cached)
    execs           <- future
    writeProfile opts (addEntries execs updated)
    deprecate inFile (profile opts)
    exitWith code
    where
    combine = if prune opts
              then \old -> union old >>= intersection
              else union
-- }}}
introText :: String
introText = unlines $ [
    versionYeganesh,
    "Usage: yeganesh [OPTIONS] [[--] DMENU_OPTIONS]",
    "OPTIONS are described below, and DMENU_OPTIONS are passed on verbatim to dmenu.",
    "Profiles are stored in the XDG data home for yeganesh."]

main :: IO ()
main = getArgs >>= either putStr runWithOptions . parseOptions introText versionYeganesh
