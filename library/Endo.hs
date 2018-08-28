module Endo
  ( main
  , mainWith
  )
where

import qualified Control.Monad as Monad
import qualified Data.Version as Version
import qualified Paths_endo as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO


main :: IO ()
main = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  config <- getConfig name arguments
  print config


data Config = Config
  { configShowHelp :: Bool
  , configShowVersion :: Bool
  } deriving Show

getConfig :: String -> [String] -> IO Config
getConfig name arguments = do
  updates <- getUpdates arguments
  let config = applyUpdates defaultConfig updates
  Monad.when (configShowHelp config) (printHelpAndExit name)
  Monad.when (configShowVersion config) printVersionAndExit
  pure config

defaultConfig :: Config
defaultConfig = Config {configShowHelp = False, configShowVersion = False}


type Update = Config -> Config

getUpdates :: [String] -> IO [Update]
getUpdates arguments = do
  let
    (updates, unexpectedArguments, unrecognizedOptions, errorMessages) =
      Console.getOpt' Console.Permute options arguments
  printUnrecognizedOptions unrecognizedOptions
  printUnexpectedArguments unexpectedArguments
  printErrorMessages errorMessages
  Monad.unless (null errorMessages) Exit.exitFailure
  pure updates

applyUpdates :: Config -> [Update] -> Config
applyUpdates = foldr id


type Option = Console.OptDescr Update

options :: [Option]
options = [helpOption, versionOption]

helpOption :: Option
helpOption = Console.Option
  ['h', '?']
  ["help"]
  (Console.NoArg (\config -> config { configShowHelp = True }))
  "show the help"

versionOption :: Option
versionOption = Console.Option
  ['v']
  ["version"]
  (Console.NoArg (\config -> config { configShowVersion = True }))
  "show the version"


printUnrecognizedOptions :: [String] -> IO ()
printUnrecognizedOptions = mapM_ printUnrecognizedOption

printUnrecognizedOption :: String -> IO ()
printUnrecognizedOption = warnLn . formatUnrecognizedOption

formatUnrecognizedOption :: String -> String
formatUnrecognizedOption unrecognizedOption =
  "WARNING: ignoring unrecognized option `" <> unrecognizedOption <> "'"


printUnexpectedArguments :: [String] -> IO ()
printUnexpectedArguments = mapM_ printUnexpectedArgument

printUnexpectedArgument :: String -> IO ()
printUnexpectedArgument = warnLn . formatUnexpectedArgument

formatUnexpectedArgument :: String -> String
formatUnexpectedArgument unexpectedArgument =
  "WARNING: ignoring unexpected argument `" <> unexpectedArgument <> "'"


printErrorMessages :: [String] -> IO ()
printErrorMessages = mapM_ printErrorMessage

printErrorMessage :: String -> IO ()
printErrorMessage = warn . formatErrorMessage

formatErrorMessage :: String -> String
formatErrorMessage = mappend "ERROR: "


printHelpAndExit :: String -> IO a
printHelpAndExit name = do
  printHelp name
  Exit.exitFailure

printHelp :: String -> IO ()
printHelp = warn . help

help :: String -> String
help name = Console.usageInfo name options


printVersionAndExit :: IO a
printVersionAndExit = do
  printVersion
  Exit.exitFailure

printVersion :: IO ()
printVersion = warnLn version

version :: String
version = Version.showVersion Package.version


warnLn :: String -> IO ()
warnLn = IO.hPutStrLn IO.stderr

warn :: String -> IO ()
warn = IO.hPutStr IO.stderr
