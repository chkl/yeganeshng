{-# LANGUAGE ScopedTypeVariables #-}
-- boilerplate {{{
module Yeganesh where

import Control.Exception (IOException, catch)
import Control.Arrow ((&&&))
import Control.Monad (filterM, liftM, when)
import Data.Char (toLower)
import Data.Function (on)
import Data.Map (Map, findWithDefault, fromList, insert, toList, union)
import Data.Time (UTCTime, diffUTCTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeDirectory, removeFile)
import System.Environment (getEnv)
import System.Environment.XDG.BaseDir (getAllDataFiles, getUserDataDir, getUserDataFile)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode, WriteMode), hClose, hPutStr, hSetEncoding, openFile, utf8)
import System.IO.Strict (hGetContents)
import Version (CurrentFormat, parseCurrentFormat, pprintCurrentFormat)
import Prelude
import Options.Applicative
import Data.List (sortOn)

data Options = Options
  { dmenuOpts :: [String],
    profile :: String,
    prune :: Bool,
    executables :: Bool
  }

pOptions :: Parser Options 
pOptions = Options <$> pDmenuOpts  <*> pProfile <*> pPrune <*> pExecutables
  where 
    pDmenuOpts = many $ argument str (metavar "DMENU_OPTIONS")
    pProfile = strOption $ long "profile"  <> short 'p' <> value "default" <> help "which popularity profile to use"
    pExecutables = switch $ long "executables" <> short 'x' <> help "search $PATH for executables for the next run"
    pPrune = switch $ long "filter" <> short 'f' <> help "prune a profile to contain exactly the lines of stdin"


type Commands = Map String Double

deprecatedDir :: IO FilePath
deprecatedDir = liftM (</> ".yeganesh") (getEnv "HOME")

inFileName :: String -> IO FilePath
inFileName arg = do
  depDir <- deprecatedDir
  dataFiles <- getAllDataFiles "yeganesh" arg
  validFiles <- filterM doesFileExist ((depDir </> arg) : dataFiles)
  case validFiles of
    [] -> getUserDataFile "yeganesh" arg
    (f : _) -> return f

outFileName :: String -> IO FilePath
outFileName arg = do
  dir <- getUserDataDir "yeganesh"
  createDirectoryIfMissing True dir
  return (dir </> arg)

deprecate :: String -> String -> IO ()
deprecate inFile arg = do
  depDir <- deprecatedDir
  when (inFile == depDir </> arg) $ do
    removeFile inFile
    filesLeft <- getDirectoryContents depDir
    when
      (null . filter (`notElem` [".", ".."]) $ filesLeft)
      (removeDirectory depDir)

readPossiblyNonExistent :: FilePath -> IO CurrentFormat
readPossiblyNonExistent file = catch (readUTF8File file) (\(_:: IOException) -> return $ "") >>= parseCurrentFormat
  where
    readUTF8File name = do
      h <- openFile name ReadMode
      hSetEncoding h utf8
      hGetContents h

writeProfile :: Options -> CurrentFormat -> IO ()
writeProfile opts cv = do
  outFile <- outFileName (profile opts)
  h <- openFile outFile WriteMode
  hSetEncoding h utf8
  hPutStr h (pprintCurrentFormat cv)
  hClose h

-- }}}
-- pure {{{
onFirst :: (a -> a') -> (a, b, c) -> (a', b, c)
onFirst f (a, b, c) = (f a, b, c)

descSnd :: Num b => (String, b) -> (b, String)
descSnd = (negate . snd) &&& (map toLower . fst)

showPriority :: Commands -> String
showPriority = unlines . map fst . sortOn descSnd . toList

-- decay exponentially, with a one-month half-life
-- The key for decay is that it be monotonic, so that commands will appear in
-- the same order before and after a decay operation; this means we can delay
-- the decay until *after* the user has selected an option.
decay :: UTCTime -> UTCTime -> Commands -> Commands
decay old new = fmap (/ factor)
  where
    seconds = fromRational . toRational $ diffUTCTime new old
    factor = exp (seconds * log 2 / 2592000)

-- clip a number to [0, 1]
clip :: (Num a, Ord a) => a -> a
clip = min 1 . max 0

-- give a boost, with things close to 0 getting a big boost, and things close
-- to 1 getting a small boost
-- Current method:
--   1. clip ]-infty, infty[ to [0, 1]
--   2. scale [0, 1] to [0.5, 1]
--   3. take sqrt; this is the boost part
--   4. scale [sqrt 0.5, 1] to [0.01, 1]
boost :: (Floating a, Ord a) => a -> a
boost = postscale . sqrt . prescale . clip
  where
    prescale = (0.5 +) . (/ 2)
    postscale = ((0.01 - s2) / ms2 +) . ((0.99 / ms2) *)
    s2 = sqrt 0.5
    ms2 = 1 - s2

geometricMean :: (Floating a, Ord a) => a -> a -> a
geometricMean a b = sqrt (clip a * clip b)

-- merge two ratings in a totally ad-hoc heuristic way
-- Current method:
--   1. clip ]-infty, infty[ to [0, 1]
--   2. scale [0, 1] to [0.5, 1]
--   3. take the geometric mean
--   4. scale [0.5, 1] to [0, 1]
merge :: (Floating a, Ord a) => a -> a -> a
merge a b = postscale (on geometricMean (prescale . clip) a b)
  where
    prescale = (0.5 +) . (/ 2)
    postscale = (* 2) . subtract 0.5

updatePriority :: String -> UTCTime -> UTCTime -> Commands -> Commands
updatePriority cmd old new cmds = insert cmd pri cmds'
  where
    cmds' = decay old new cmds
    pri = boost $ findWithDefault 0 cmd cmds'

parseInput :: String -> Commands
parseInput = fromList . flip zip (repeat 0) . filter (not . null) . lines

-- dmenu <=4.3.1 did not print a newline after the selection; dmenu >= 4.4
-- does. So we need to normalize the string to match between the two versions.
stripNewline :: String -> String
stripNewline "" = ""
stripNewline "\n" = ""
stripNewline (x : xs) = x : stripNewline xs

-- an (almost certainly buggy) internal implementation of the shell's IFS
-- mechanism
xCons :: a -> [[a]] -> [[a]]
xCons x (p : ps) = ((x : p) : ps)
xCons x [] = [[x]]

parsePath :: String -> [String]
parsePath ('\\' : x : xs) = xCons x (parsePath xs)
parsePath (':' : xs) = [] : parsePath xs
parsePath (x : xs) = xCons x (parsePath xs)
parsePath [] = []

addEntries :: [String] -> CurrentFormat -> CurrentFormat
addEntries es (t, m) = (t, union m (fromList [(e, 0) | e <- es]))

-- }}}
