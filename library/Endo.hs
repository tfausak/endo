-- | Endo parses and generates [Rocket League](https://www.rocketleague.com)
-- replays. Parsing replays can be used to analyze data in order to collect
-- high-level statistics like players and points, or low-level details like
-- positions and cameras. Generating replays can be used to modify replays in
-- order to force everyone into the same car or change the map a game was
-- played on.
--
-- Endo supports every version of Rocket League up to 1.52, also known as the
-- "progression update". If a replay can be played by the Rocket League client,
-- it can be parsed by Endo.
--
-- Endo is a command-line application. You should only use it if you're
-- comfortable running things in terminals or command prompts. Otherwise
-- consider using another tool like [Ball Chasing](https://ballchasing.com).
--
-- If you're looking for replay files on your machine, they can be found in the
-- following directories:
--
-- [Windows]: [ ](#haddock-requires-something-here)
--
--     > %UserProfile%\Documents\My Games\Rocket League\TAGame\Demos
--
--     For example:
--
--     > C:\Users\Taylor\Documents\My Games\Rocket League\TAGame\Demos
--
-- [MacOS]: [ ](#haddock-requires-something-here)
--
--     > $HOME/Library/Application Support/Rocket League/TAGame/Demos
--
--     For example:
--
--     > /Users/taylor/Library/Application Support/Rocket League/TAGame/Demos
--
-- [Linux]: [ ](#haddock-requires-something-here)
--
--     > $HOME/.local/share/Rocket League/TAGame/Demos
--
--     For example:
--
--     > /home/taylor/.local/share/Rocket League/TAGame/Demos
module Endo
  ( main
  , mainWith
  , replayFromBinary
  , replayToJson
  , replayFromJson
  , replayToBinary
  , Replay(..)
  , Section(..)
  , Header(..)
  , Content(..)
  , Optional(..)
  , U32(..)
  , Base64(..)
  )
where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified Data.Word as Word
import qualified Paths_endo as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Printf as Printf


-- | == Options
--
-- Endo accepts a variety of command-line options. Usually you only need to
-- specify the @--input@ option and sometimes the @--output@ option.
--
-- [@--help@, @-h@, @-?@] Prints Endo's help to 'IO.stderr' and exits with a
-- non-zero status code.
--
-- [@--version@, @-v@] Prints Endo's version number to 'IO.stderr' and exits
-- with a non-zero status code.
--
-- [@--input FILE@, @-i FILE@] Specifies the input file. If this option is not
-- given, input will be read from 'IO.stdin'.
--
-- [@--output FILE@, @-o FILE@] Specifies the output file. If this option is
-- not given, output will be written to 'IO.stdout'.
--
-- [@--mode MODE@, @-m MODE@] Specifies the mode, either @decode@ or @encode@.
--
--     -   @decode@ converts a binary @.replay@ file into JSON. In other words,
--         first it calls 'replayFromBinary' and then it calls 'replayToJson'.
--
--     -   @encode@ does the opposite; it converts JSON into a binary @.replay@
--         file. In other words, first it calls 'replayFromJson' and then it
--         calls 'replayToBinary'.
--
--     Usually you do not need to manually set the mode. If this option is not
--     given, the mode is chosen automatically following these rules:
--
--      1.  If the input file ends with @.json@: @encode@.
--      2.  If the input file ends with @.replay@: @decode@.
--      3.  If the output file ends with @.json@: @decode@.
--      4.  If the output file ends with @.replay@: @encode@.
--      5.  Otherwise: @decode@.
--
-- == Examples
--
-- Each group of examples shows many ways to do the same thing. If you're
-- running Endo on Windows, you should prefer @--input INPUT@ and
-- @--output OUTPUT@ to @< INPUT@ and @> OUTPUT@ respectively. Windows attempts
-- to encode pipes as UTF-16, which can give unexpected results.
--
-- > $ endo < example.replay > example.json
-- > $ endo -i example.replay -o example.json
-- > $ endo --mode decode --input example.replay --output example.json
--
-- Decodes a binary replay file and outputs a JSON replay file. This is by far
-- the most common way to use Endo.
--
-- > $ endo < example.replay
-- > $ endo -i example.replay
-- > $ endo --mode decode --input example.replay
--
-- Decodes a binary replay file and outputs JSON replay to 'IO.stdout'. Note
-- that in the first example @--mode@ is optional because the default mode is
-- @decode@.
--
-- > $ endo -m encode < example.json > example.replay
-- > $ endo -i example.json -o example.replay
-- > $ endo --mode encode --input example.json --output example.replay
--
-- Encodes a JSON replay file and outputs a binary replay file.
--
-- > $ endo -m encode < example.json
-- > $ endo -i example.json
-- > $ endo --mode encode --input example.json
--
-- Encodes a JSON replay file and outputs a binary replay to 'IO.stdout'. Note
-- that in the first example @--mode@ is required, otherwise it would default
-- to @decode@. Endo does not know the name of the file piped to 'IO.stdin'.
main :: IO ()
main = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

-- | This helper function allows you to call 'main' as if it was a function
-- with whatever arguments you want. This is primarily used for testing.
mainWith
  :: String
  -- ^ Program name, like @"endo"@. This is only used when outputting the help,
  -- so you can pass the empty string if you don't need to show the help.
  -- Otherwise this usually comes from 'Environment.getProgName'.
  -> [String]
  -- ^ Command-line arguments, like @["--input", "example.replay"]@. These
  -- usually come from 'Environment.getArgs'.
  -> IO ()
mainWith name arguments = do
  config <- getConfig name arguments
  input <- getInput $ configInputFile config
  let mode = getMode config
  replay <- either printErrorMessageAndExit pure $ decodeWith mode input
  let output = encodeWith mode replay
  putOutput (configOutputFile config) output


-- | Decodes a binary replay. This is the opposite of 'replayToBinary'. Note
-- that converting from binary and then back to binary is /not/ guaranteed to
-- give you back what you started with (unlike 'replayFromJson'). The result
-- should be effectively the same, but the actual bytes might differ slightly.
replayFromBinary :: Bytes.ByteString -> Either String Replay
replayFromBinary =
  either (Left . third) (Right . third)
    . Binary.decodeOrFail
    . LazyBytes.fromStrict

-- | Encodes a JSON replay. This is the opposite of 'replayFromJson'.
replayToJson :: Replay -> Bytes.ByteString
replayToJson = LazyBytes.toStrict . Aeson.encode

-- | Decodes a JSON replay. This is the opposite of 'replayToJson'. Note that
-- converting from JSON and then back to JSON /is/ guaranteed to give you back
-- what you started with (unlike 'replayFromBinary').
replayFromJson :: Bytes.ByteString -> Either String Replay
replayFromJson = Aeson.eitherDecodeStrict'

-- | Encodes a binary replay. This is the opposite of 'replayFromBinary'.
replayToBinary :: Replay -> Bytes.ByteString
replayToBinary = LazyBytes.toStrict . Binary.encode


-- | A Rocket League replay. Most of the information you'll usually want, like
-- the stuff shown on the scoreboard, is in the 'Header'. You'll typically only
-- need the 'Content' if you want to analyze the game data that's sent over the
-- network.
data Replay = Replay
  { replayHeader :: Section Header
  , replayContent :: Section Content
  }

instance Binary.Binary Replay where
  get = Binary.label "Replay" $ Replay <$> Binary.get <*> Binary.get
  put replay =
    Binary.put (replayHeader replay) <> Binary.put (replayContent replay)

instance Aeson.FromJSON Replay where
  parseJSON = Aeson.withObject "Replay" $ \object ->
    Replay <$> requiredKey object "header" <*> requiredKey object "content"

instance Aeson.ToJSON Replay where
  toEncoding replay =
    Aeson.pairs $ toPair "header" (replayHeader replay) <> toPair
      "content"
      (replayContent replay)
  toJSON replay = Aeson.object
    [ toPair "header" $ replayHeader replay
    , toPair "content" $ replayContent replay
    ]


-- | A high-level section of a replay. Rocket League replays are split up into
-- two sections, each with a size and CRC. The instances for this type handle
-- the size and CRC so that you don't have to worry about them.
newtype Section a
  = Section a

instance Binary.Binary a => Binary.Binary (Section a) where
  get = Binary.label "Section" $ do
    size <- Binary.get
    expectedCrc <- Binary.get
    bytes <- Binary.getByteString . word32ToInt $ u32ToWord32 size
    let actualCrc = word32ToU32 $ crc32Bytes crc32Table crc32Initial bytes
    Monad.when (u32ToWord32 actualCrc /= u32ToWord32 expectedCrc) . fail
      $ "actual CRC "
      <> show actualCrc
      <> " does not match expected CRC "
      <> show expectedCrc
    Section <$> either
      (fail . third)
      (pure . third)
      (Binary.decodeOrFail $ LazyBytes.fromStrict bytes)
  put section =
    let
      bytes =
        LazyBytes.toStrict . Binary.runPut . Binary.put $ unwrapSection section
    in
      Binary.put (word32ToU32 . intToWord32 $ Bytes.length bytes)
      <> Binary.put (word32ToU32 $ crc32Bytes crc32Table crc32Initial bytes)
      <> Binary.putByteString bytes

instance Aeson.FromJSON a => Aeson.FromJSON (Section a) where
  parseJSON = fmap Section . Aeson.parseJSON

instance Aeson.ToJSON a => Aeson.ToJSON (Section a) where
  toEncoding = Aeson.toEncoding . unwrapSection
  toJSON = Aeson.toJSON . unwrapSection

unwrapSection :: Section a -> a
unwrapSection (Section a) = a


-- | The header or "meta" replay information. This includes everything that is
-- shown in the in-game replay menu and then some.
data Header = Header
  { headerMajorVersion :: U32
  -- ^ The major or "engine" version number. Note that this isn't the same as
  -- Rocket League's marketing version number, which is usually something like
  -- "v1.52". You may be able to convert between the two, but there's no
  -- guarantee that it's a one-to-one mapping.
  , headerMinorVersion :: U32
  -- ^ The minor or "licensee" version number.
  , headerPatchVersion :: Optional U32
  -- ^ The patch or "net" version number. Replays before v1.35 (the
  -- "anniversary update") don't have this field.
  , headerRest :: Base64
  }

instance Binary.Binary Header where
  get = Binary.label "Header" $ do
    majorVersion <- Binary.get
    minorVersion <- Binary.get
    Header majorVersion minorVersion
      <$> getOptional (hasPatchVersion majorVersion minorVersion)
      <*> Binary.get
  put header =
    Binary.put (headerMajorVersion header)
      <> Binary.put (headerMinorVersion header)
      <> putOptional (headerPatchVersion header)
      <> Binary.put (headerRest header)

instance Aeson.FromJSON Header where
  parseJSON = Aeson.withObject "Header" $ \object ->
    Header
      <$> requiredKey object "majorVersion"
      <*> requiredKey object "minorVersion"
      <*> optionalKey object "patchVersion"
      <*> requiredKey object "rest"

instance Aeson.ToJSON Header where
  toEncoding header =
    Aeson.pairs
      $ toPair "majorVersion" (headerMajorVersion header)
      <> toPair "minorVersion" (headerMinorVersion header)
      <> toPair "patchVersion" (headerPatchVersion header)
      <> toPair "rest" (headerRest header)
  toJSON header = Aeson.object
    [ toPair "majorVersion" $ headerMajorVersion header
    , toPair "minorVersion" $ headerMinorVersion header
    , toPair "patchVersion" $ headerPatchVersion header
    , toPair "rest" $ headerRest header
    ]

hasPatchVersion :: U32 -> U32 -> Bool
hasPatchVersion majorVersion minorVersion =
  u32ToWord32 majorVersion >= 868 && u32ToWord32 minorVersion >= 18


-- | The content or "data" replay information. This includes everything that is
-- shown when watching a replay in game.
newtype Content
  = Content Base64

instance Binary.Binary Content where
  get = Content <$> Binary.get
  put = Binary.put . unwrapContent

instance Aeson.FromJSON Content where
  parseJSON = fmap Content . Aeson.parseJSON

instance Aeson.ToJSON Content where
  toEncoding = Aeson.toEncoding . unwrapContent
  toJSON = Aeson.toJSON . unwrapContent

unwrapContent :: Content -> Base64
unwrapContent (Content base64) = base64


-- | A value that may or may not exist. This type mostly exists to /avoid/ the
-- 'Binary.Binary' and 'Aeson.FromJSON' instances for 'Maybe'. The
-- 'Binary.Binary' instance is troublesome because in Rocket League replays
-- typically the lack of a value is not encoded at all. The 'Aeson.FromJSON'
-- instance is troublesome because missing a key is treated differently than a
-- key existing but the value being 'Aeson.Null'.
newtype Optional a
  = Optional (Maybe a)

instance Aeson.ToJSON a => Aeson.ToJSON (Optional a) where
  toEncoding = Aeson.toEncoding . optionalToMaybe
  toJSON = Aeson.toJSON . optionalToMaybe

optionalToMaybe :: Optional a -> Maybe a
optionalToMaybe (Optional a) = a

maybeToOptional :: Maybe a -> Optional a
maybeToOptional = Optional

getOptional :: Binary.Binary a => Bool -> Binary.Get (Optional a)
getOptional condition =
  maybeToOptional <$> if condition then Just <$> Binary.get else pure Nothing

putOptional :: Binary.Binary a => Optional a -> Binary.Put
putOptional = maybe mempty Binary.put . optionalToMaybe


-- | A 32-bit unsigned integer stored in little-endian byte order.
newtype U32
  = U32 Word.Word32

instance Binary.Binary U32 where
  get = word32ToU32 <$> Binary.getWord32le
  put = Binary.putWord32le . u32ToWord32

instance Aeson.FromJSON U32 where
  parseJSON = fmap word32ToU32 . Aeson.parseJSON

instance Show U32 where
  show = Printf.printf "0x%08x" . u32ToWord32

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
  get = Base64 . LazyBytes.toStrict <$> Binary.getRemainingLazyByteString
  put = Binary.putByteString . unwrapBase64

instance Aeson.FromJSON Base64 where
  parseJSON =
    Aeson.withText "Base64"
      $ either fail (pure . Base64)
      . Base64.decode
      . Text.encodeUtf8

instance Aeson.ToJSON Base64 where
  toEncoding =
    Aeson.toEncoding . Text.decodeUtf8 . Base64.encode . unwrapBase64
  toJSON = Aeson.toJSON . Text.decodeUtf8 . Base64.encode . unwrapBase64

unwrapBase64 :: Base64 -> Bytes.ByteString
unwrapBase64 (Base64 bytes) = bytes


getInput :: Maybe FilePath -> IO Bytes.ByteString
getInput = maybe Bytes.getContents Bytes.readFile

decodeWith :: Mode -> Bytes.ByteString -> Either String Replay
decodeWith mode = case mode of
  ModeDecode -> replayFromBinary
  ModeEncode -> replayFromJson

encodeWith :: Mode -> Replay -> Bytes.ByteString
encodeWith mode = case mode of
  ModeDecode -> replayToJson
  ModeEncode -> replayToBinary

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
  config <- either printErrorMessageAndExit pure
    $ applyUpdates defaultConfig updates
  Monad.when (configShowHelp config) $ printHelpAndExit name
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
  _ -> Left $ "invalid mode `" <> mode <> "'"

getMode :: Config -> Mode
getMode config =
  Maybe.fromMaybe (Maybe.fromMaybe ModeDecode $ implicitMode config)
    $ configMode config

implicitMode :: Config -> Maybe Mode
implicitMode config = case configInputFile config of
  Just inputFile
    | hasExtension ".json" inputFile -> Just ModeEncode
    | hasExtension ".replay" inputFile -> Just ModeDecode
  _ -> case configOutputFile config of
    Just outputFile
      | hasExtension ".json" outputFile -> Just ModeDecode
      | hasExtension ".replay" outputFile -> Just ModeEncode
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

makeOption :: String -> [String] -> String -> Argument -> Option
makeOption short long description argument =
  Console.Option short long argument description

type Argument = Console.ArgDescr Update

makeArgument :: String -> (String -> Update) -> Argument
makeArgument = flip Console.ReqArg

options :: [Option]
options = [helpOption, inputOption, modeOption, outputOption, versionOption]

helpOption :: Option
helpOption =
  makeOption "h?" ["help"] "show the help" . Console.NoArg $ \config ->
    Right config { configShowHelp = True }

inputOption :: Option
inputOption =
  makeOption "i" ["input"] "the input file"
    . makeArgument "FILE"
    $ \input config -> Right config { configInputFile = Just input }

modeOption :: Option
modeOption =
  makeOption "m" ["mode"] "decode or encode"
    . makeArgument "MODE"
    $ \string config ->
        (\mode -> config { configMode = Just mode }) <$> parseMode string

outputOption :: Option
outputOption =
  makeOption "o" ["output"] "the output file"
    . makeArgument "FILE"
    $ \output config -> Right config { configOutputFile = Just output }

versionOption :: Option
versionOption =
  makeOption "v" ["version"] "show the version" . Console.NoArg $ \config ->
    Right config { configShowVersion = True }


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
help name = Console.usageInfo (name <> " version " <> version) options


printVersionAndExit :: IO a
printVersionAndExit = dieLn version

version :: String
version = Version.showVersion Package.version


crc32Bytes
  :: Vector.Vector Word.Word32
  -> Word.Word32
  -> Bytes.ByteString
  -> Word.Word32
crc32Bytes table initial =
  Bits.complement . Bytes.foldl (crc32Update table) (Bits.complement initial)

crc32Initial :: Word.Word32
crc32Initial = 0xefcbf201

crc32Table :: Vector.Vector Word.Word32
crc32Table = Vector.fromList
  [ 0x00000000
  , 0x04c11db7
  , 0x09823b6e
  , 0x0d4326d9
  , 0x130476dc
  , 0x17c56b6b
  , 0x1a864db2
  , 0x1e475005
  , 0x2608edb8
  , 0x22c9f00f
  , 0x2f8ad6d6
  , 0x2b4bcb61
  , 0x350c9b64
  , 0x31cd86d3
  , 0x3c8ea00a
  , 0x384fbdbd
  , 0x4c11db70
  , 0x48d0c6c7
  , 0x4593e01e
  , 0x4152fda9
  , 0x5f15adac
  , 0x5bd4b01b
  , 0x569796c2
  , 0x52568b75
  , 0x6a1936c8
  , 0x6ed82b7f
  , 0x639b0da6
  , 0x675a1011
  , 0x791d4014
  , 0x7ddc5da3
  , 0x709f7b7a
  , 0x745e66cd
  , 0x9823b6e0
  , 0x9ce2ab57
  , 0x91a18d8e
  , 0x95609039
  , 0x8b27c03c
  , 0x8fe6dd8b
  , 0x82a5fb52
  , 0x8664e6e5
  , 0xbe2b5b58
  , 0xbaea46ef
  , 0xb7a96036
  , 0xb3687d81
  , 0xad2f2d84
  , 0xa9ee3033
  , 0xa4ad16ea
  , 0xa06c0b5d
  , 0xd4326d90
  , 0xd0f37027
  , 0xddb056fe
  , 0xd9714b49
  , 0xc7361b4c
  , 0xc3f706fb
  , 0xceb42022
  , 0xca753d95
  , 0xf23a8028
  , 0xf6fb9d9f
  , 0xfbb8bb46
  , 0xff79a6f1
  , 0xe13ef6f4
  , 0xe5ffeb43
  , 0xe8bccd9a
  , 0xec7dd02d
  , 0x34867077
  , 0x30476dc0
  , 0x3d044b19
  , 0x39c556ae
  , 0x278206ab
  , 0x23431b1c
  , 0x2e003dc5
  , 0x2ac12072
  , 0x128e9dcf
  , 0x164f8078
  , 0x1b0ca6a1
  , 0x1fcdbb16
  , 0x018aeb13
  , 0x054bf6a4
  , 0x0808d07d
  , 0x0cc9cdca
  , 0x7897ab07
  , 0x7c56b6b0
  , 0x71159069
  , 0x75d48dde
  , 0x6b93dddb
  , 0x6f52c06c
  , 0x6211e6b5
  , 0x66d0fb02
  , 0x5e9f46bf
  , 0x5a5e5b08
  , 0x571d7dd1
  , 0x53dc6066
  , 0x4d9b3063
  , 0x495a2dd4
  , 0x44190b0d
  , 0x40d816ba
  , 0xaca5c697
  , 0xa864db20
  , 0xa527fdf9
  , 0xa1e6e04e
  , 0xbfa1b04b
  , 0xbb60adfc
  , 0xb6238b25
  , 0xb2e29692
  , 0x8aad2b2f
  , 0x8e6c3698
  , 0x832f1041
  , 0x87ee0df6
  , 0x99a95df3
  , 0x9d684044
  , 0x902b669d
  , 0x94ea7b2a
  , 0xe0b41de7
  , 0xe4750050
  , 0xe9362689
  , 0xedf73b3e
  , 0xf3b06b3b
  , 0xf771768c
  , 0xfa325055
  , 0xfef34de2
  , 0xc6bcf05f
  , 0xc27dede8
  , 0xcf3ecb31
  , 0xcbffd686
  , 0xd5b88683
  , 0xd1799b34
  , 0xdc3abded
  , 0xd8fba05a
  , 0x690ce0ee
  , 0x6dcdfd59
  , 0x608edb80
  , 0x644fc637
  , 0x7a089632
  , 0x7ec98b85
  , 0x738aad5c
  , 0x774bb0eb
  , 0x4f040d56
  , 0x4bc510e1
  , 0x46863638
  , 0x42472b8f
  , 0x5c007b8a
  , 0x58c1663d
  , 0x558240e4
  , 0x51435d53
  , 0x251d3b9e
  , 0x21dc2629
  , 0x2c9f00f0
  , 0x285e1d47
  , 0x36194d42
  , 0x32d850f5
  , 0x3f9b762c
  , 0x3b5a6b9b
  , 0x0315d626
  , 0x07d4cb91
  , 0x0a97ed48
  , 0x0e56f0ff
  , 0x1011a0fa
  , 0x14d0bd4d
  , 0x19939b94
  , 0x1d528623
  , 0xf12f560e
  , 0xf5ee4bb9
  , 0xf8ad6d60
  , 0xfc6c70d7
  , 0xe22b20d2
  , 0xe6ea3d65
  , 0xeba91bbc
  , 0xef68060b
  , 0xd727bbb6
  , 0xd3e6a601
  , 0xdea580d8
  , 0xda649d6f
  , 0xc423cd6a
  , 0xc0e2d0dd
  , 0xcda1f604
  , 0xc960ebb3
  , 0xbd3e8d7e
  , 0xb9ff90c9
  , 0xb4bcb610
  , 0xb07daba7
  , 0xae3afba2
  , 0xaafbe615
  , 0xa7b8c0cc
  , 0xa379dd7b
  , 0x9b3660c6
  , 0x9ff77d71
  , 0x92b45ba8
  , 0x9675461f
  , 0x8832161a
  , 0x8cf30bad
  , 0x81b02d74
  , 0x857130c3
  , 0x5d8a9099
  , 0x594b8d2e
  , 0x5408abf7
  , 0x50c9b640
  , 0x4e8ee645
  , 0x4a4ffbf2
  , 0x470cdd2b
  , 0x43cdc09c
  , 0x7b827d21
  , 0x7f436096
  , 0x7200464f
  , 0x76c15bf8
  , 0x68860bfd
  , 0x6c47164a
  , 0x61043093
  , 0x65c52d24
  , 0x119b4be9
  , 0x155a565e
  , 0x18197087
  , 0x1cd86d30
  , 0x029f3d35
  , 0x065e2082
  , 0x0b1d065b
  , 0x0fdc1bec
  , 0x3793a651
  , 0x3352bbe6
  , 0x3e119d3f
  , 0x3ad08088
  , 0x2497d08d
  , 0x2056cd3a
  , 0x2d15ebe3
  , 0x29d4f654
  , 0xc5a92679
  , 0xc1683bce
  , 0xcc2b1d17
  , 0xc8ea00a0
  , 0xd6ad50a5
  , 0xd26c4d12
  , 0xdf2f6bcb
  , 0xdbee767c
  , 0xe3a1cbc1
  , 0xe760d676
  , 0xea23f0af
  , 0xeee2ed18
  , 0xf0a5bd1d
  , 0xf464a0aa
  , 0xf9278673
  , 0xfde69bc4
  , 0x89b8fd09
  , 0x8d79e0be
  , 0x803ac667
  , 0x84fbdbd0
  , 0x9abc8bd5
  , 0x9e7d9662
  , 0x933eb0bb
  , 0x97ffad0c
  , 0xafb010b1
  , 0xab710d06
  , 0xa6322bdf
  , 0xa2f33668
  , 0xbcb4666d
  , 0xb8757bda
  , 0xb5365d03
  , 0xb1f740b4
  ]

crc32Update
  :: Vector.Vector Word.Word32 -> Word.Word32 -> Word.Word8 -> Word.Word32
crc32Update table crc byte = do
  let
    index = word8ToInt . Bits.xor byte . word32ToWord8 $ Bits.shiftR crc 24
    left = table Vector.! index
    right = Bits.shiftL crc 8
  Bits.xor left right


dieLn :: String -> IO a
dieLn = Exit.die

die :: String -> IO a
die message = do
  warn message
  Exit.exitFailure

hasExtension :: String -> FilePath -> Bool
hasExtension = List.isSuffixOf

intToWord32 :: Int -> Word.Word32
intToWord32 = fromIntegral

optionalKey
  :: Aeson.FromJSON v => Aeson.Object -> String -> Aeson.Parser (Optional v)
optionalKey object key = maybeToOptional <$> object Aeson..:? Text.pack key

requiredKey :: Aeson.FromJSON v => Aeson.Object -> String -> Aeson.Parser v
requiredKey object key = object Aeson..: Text.pack key

third :: (a, b, c) -> c
third (_, _, c) = c

toPair :: (Aeson.ToJSON v, Aeson.KeyValue p) => String -> v -> p
toPair key value = Text.pack key Aeson..= value

warnLn :: String -> IO ()
warnLn = IO.hPutStrLn IO.stderr

warn :: String -> IO ()
warn = IO.hPutStr IO.stderr

word8ToInt :: Word.Word8 -> Int
word8ToInt = fromIntegral

word32ToInt :: Word.Word32 -> Int
word32ToInt = fromIntegral

word32ToWord8 :: Word.Word32 -> Word.Word8
word32ToWord8 = fromIntegral
