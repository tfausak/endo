module Endo
  ( main
  , mainWith
  )
where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Encoding
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
  input <- getInput (configInputFile config)
  let mode = getMode config
  replay <- either printErrorMessageAndExit pure (decodeWith mode input)
  let output = encodeWith mode replay
  putOutput (configOutputFile config) output


newtype Replay
  = Replay Bytes.ByteString

instance Aeson.FromJSON Replay where
  parseJSON = Aeson.withText "Replay"
    (either fail (pure . Replay) . Base64.decode . Encoding.encodeUtf8)

instance Aeson.ToJSON Replay where
  toEncoding (Replay bytes) =
    Aeson.toEncoding (Encoding.decodeUtf8 (Base64.encode bytes))
  toJSON (Replay bytes) =
    Aeson.toJSON (Encoding.decodeUtf8 (Base64.encode bytes))


replayToBytes :: Replay -> Bytes.ByteString
replayToBytes (Replay bytes) = bytes

replayToJson :: Replay -> Bytes.ByteString
replayToJson = LazyBytes.toStrict . Aeson.encode

replayFromBytes :: Bytes.ByteString -> Either String Replay
replayFromBytes = Right . Replay

replayFromJson :: Bytes.ByteString -> Either String Replay
replayFromJson = Aeson.eitherDecodeStrict'


getInput :: Maybe FilePath -> IO Bytes.ByteString
getInput = maybe Bytes.getContents Bytes.readFile

decodeWith :: Mode -> Bytes.ByteString -> Either String Replay
decodeWith mode = case mode of
  ModeDecode -> replayFromBytes
  ModeEncode -> replayFromJson

encodeWith :: Mode -> Replay -> Bytes.ByteString
encodeWith mode = case mode of
  ModeDecode -> replayToJson
  ModeEncode -> replayToBytes

putOutput :: Maybe FilePath -> Bytes.ByteString -> IO ()
putOutput = maybe Bytes.putStr Bytes.writeFile


data Config = Config
  { configInputFile :: Maybe FilePath
  , configOutputFile :: Maybe FilePath
  , configMode :: Maybe Mode
  , configShowHelp :: Bool
  , configShowVersion :: Bool
  }

getConfig :: String -> [String] -> IO Config
getConfig name arguments = do
  updates <- getUpdates arguments
  config <- either
    printErrorMessageAndExit
    pure
    (applyUpdates defaultConfig updates)
  Monad.when (configShowHelp config) (printHelpAndExit name)
  Monad.when (configShowVersion config) printVersionAndExit
  pure config

defaultConfig :: Config
defaultConfig = Config
  { configInputFile = Nothing
  , configMode = Nothing
  , configOutputFile = Nothing
  , configShowHelp = False
  , configShowVersion = False
  }


data Mode
  = ModeDecode
  | ModeEncode

parseMode :: String -> Either String Mode
parseMode mode = case mode of
  "decode" -> Right ModeDecode
  "encode" -> Right ModeEncode
  _ -> Left ("invalid mode `" <> mode <> "'")

getMode :: Config -> Mode
getMode config = Maybe.fromMaybe
  (Maybe.fromMaybe ModeDecode (implicitMode config))
  (configMode config)

implicitMode :: Config -> Maybe Mode
implicitMode config = case fmap takeExtension (configInputFile config) of
  Just ".json" -> Just ModeEncode
  Just ".replay" -> Just ModeDecode
  _ -> case fmap takeExtension (configOutputFile config) of
    Just ".json" -> Just ModeDecode
    Just ".replay" -> Just ModeEncode
    _ -> Nothing

takeExtension :: FilePath -> String
takeExtension = dropWhile (/= '.')


type Update = Config -> Either String Config

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

applyUpdates :: Config -> [Update] -> Either String Config
applyUpdates = Monad.foldM applyUpdate

applyUpdate :: Config -> Update -> Either String Config
applyUpdate config update = update config


type Option = Console.OptDescr Update

options :: [Option]
options = [helpOption, inputOption, modeOption, outputOption, versionOption]

helpOption :: Option
helpOption = Console.Option
  ['h', '?']
  ["help"]
  (Console.NoArg (\config -> Right config { configShowHelp = True }))
  "show the help"

inputOption :: Option
inputOption = Console.Option
  ['i']
  ["input"]
  (Console.ReqArg
    (\input config -> Right config { configInputFile = Just input })
    "FILE"
  )
  "the input file"

modeOption :: Option
modeOption = Console.Option
  ['m']
  ["mode"]
  (Console.ReqArg
    (\string config ->
      fmap (\mode -> config { configMode = Just mode }) (parseMode string)
    )
    "MODE"
  )
  "decode or encode"

outputOption :: Option
outputOption = Console.Option
  ['o']
  ["output"]
  (Console.ReqArg
    (\output config -> Right config { configOutputFile = Just output })
    "FILE"
  )
  "the output file"

versionOption :: Option
versionOption = Console.Option
  ['v']
  ["version"]
  (Console.NoArg (\config -> Right config { configShowVersion = True }))
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

printErrorMessageAndExit :: String -> IO a
printErrorMessageAndExit = dieLn . formatErrorMessage

printErrorMessage :: String -> IO ()
printErrorMessage = warn . formatErrorMessage

formatErrorMessage :: String -> String
formatErrorMessage = mappend "ERROR: "


printHelpAndExit :: String -> IO a
printHelpAndExit = die . help

help :: String -> String
help name = Console.usageInfo name options


printVersionAndExit :: IO a
printVersionAndExit = dieLn version

version :: String
version = Version.showVersion Package.version


dieLn :: String -> IO a
dieLn = Exit.die

die :: String -> IO a
die message = do
  warn message
  Exit.exitFailure


warnLn :: String -> IO ()
warnLn = IO.hPutStrLn IO.stderr

warn :: String -> IO ()
warn = IO.hPutStr IO.stderr
