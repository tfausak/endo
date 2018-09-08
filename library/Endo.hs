module Endo
  ( main
  , mainWith
  )
where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Data.Word as Word
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


data Replay = Replay
  { replayHeader :: Section Base64
  , replayContent :: Section Base64
  }

instance Binary.Binary Replay where
  get = Replay <$> Binary.get <*> Binary.get
  put replay =
    Binary.put (replayHeader replay) <> Binary.put (replayContent replay)

instance Aeson.FromJSON Replay where
  parseJSON = Aeson.withObject
    "Replay"
    (\object ->
      Replay <$> requiredKey object "header" <*> requiredKey object "content"
    )

instance Aeson.ToJSON Replay where
  toEncoding replay = Aeson.pairs
    (toPair "header" (replayHeader replay)
    <> toPair "content" (replayContent replay)
    )
  toJSON replay = Aeson.object
    [ toPair "header" (replayHeader replay)
    , toPair "content" (replayContent replay)
    ]


data Section a = Section
  { sectionSize :: U32
  , sectionCrc :: U32
  , sectionValue :: a
  }

instance Binary.Binary a => Binary.Binary (Section a) where
  get = do
    size <- Binary.get
    Section size
      <$> Binary.get
      <*> Binary.isolate (word32ToInt (u32ToWord32 size)) Binary.get
  put section =
    Binary.put (sectionSize section)
      <> Binary.put (sectionCrc section)
      <> Binary.put (sectionValue section)

instance Aeson.FromJSON a => Aeson.FromJSON (Section a) where
  parseJSON = Aeson.withObject
    "Section"
    (\object ->
      Section
        <$> requiredKey object "size"
        <*> requiredKey object "crc"
        <*> requiredKey object "value"
    )

instance Aeson.ToJSON a => Aeson.ToJSON (Section a) where
  toEncoding section = Aeson.pairs
    (toPair "size" (sectionSize section)
    <> toPair "crc" (sectionCrc section)
    <> toPair "value" (sectionValue section)
    )
  toJSON section = Aeson.object
    [ toPair "size" (sectionSize section)
    , toPair "crc" (sectionCrc section)
    , toPair "value" (sectionValue section)
    ]


newtype U32
  = U32 Word.Word32

instance Binary.Binary U32 where
  get = fmap word32ToU32 Binary.getWord32le
  put = Binary.putWord32le . u32ToWord32

instance Aeson.FromJSON U32 where
  parseJSON = fmap word32ToU32 . Aeson.parseJSON

instance Aeson.ToJSON U32 where
  toEncoding = Aeson.toEncoding . u32ToWord32
  toJSON = Aeson.toJSON . u32ToWord32

word32ToU32 :: Word.Word32 -> U32
word32ToU32 = U32

u32ToWord32 :: U32 -> Word.Word32
u32ToWord32 (U32 word32) = word32


newtype Base64
  = Base64 Bytes.ByteString

instance Binary.Binary Base64 where
  get = fmap (Base64 . LazyBytes.toStrict) Binary.getRemainingLazyByteString
  put (Base64 bytes) = Binary.putByteString bytes

instance Aeson.FromJSON Base64 where
  parseJSON = Aeson.withText "Base64"
    (either fail (pure . Base64) . Base64.decode . Text.encodeUtf8)

instance Aeson.ToJSON Base64 where
  toEncoding (Base64 bytes) =
    Aeson.toEncoding (Text.decodeUtf8 (Base64.encode bytes))
  toJSON (Base64 bytes) =
    Aeson.toJSON (Text.decodeUtf8 (Base64.encode bytes))


replayToBytes :: Replay -> Bytes.ByteString
replayToBytes = LazyBytes.toStrict . Binary.encode

replayToJson :: Replay -> Bytes.ByteString
replayToJson = LazyBytes.toStrict . Aeson.encode

replayFromBytes :: Bytes.ByteString -> Either String Replay
replayFromBytes =
  either (Left . third) (Right . third)
    . Binary.decodeOrFail
    . LazyBytes.fromStrict

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

requiredKey :: Aeson.FromJSON v => Aeson.Object -> String -> Aeson.Parser v
requiredKey object key = object Aeson..: Text.pack key

takeExtension :: FilePath -> String
takeExtension = dropWhile (/= '.')

third :: (a, b, c) -> c
third (_, _, c) = c

toPair :: (Aeson.ToJSON v, Aeson.KeyValue p) => String -> v -> p
toPair key value = Text.pack key Aeson..= value

warnLn :: String -> IO ()
warnLn = IO.hPutStrLn IO.stderr

warn :: String -> IO ()
warn = IO.hPutStr IO.stderr

word32ToInt :: Word.Word32 -> Int
word32ToInt = fromIntegral
