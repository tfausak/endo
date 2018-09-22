{-# LANGUAGE OverloadedStrings #-}

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
  , Property(..)
  , Content(..)
  , KeyFrame(..)
  , Frames(..)
  , Message(..)
  , Mark(..)
  , ClassMapping(..)
  , Cache(..)
  , AttributeMapping(..)
  )
where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.Bool as Bool
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Int as Int
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
import qualified Unsafe.Coerce as Unsafe


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
--     If you're looking for replay files on your machine, they can be found in
--     the following directories:
--
--     [Windows]: [ ](#haddock-requires-something-here)
--
--         > %UserProfile%\Documents\My Games\Rocket League\TAGame\Demos
--
--         For example:
--
--         > C:\Users\Taylor\Documents\My Games\Rocket League\TAGame\Demos
--
--     [MacOS]: [ ](#haddock-requires-something-here)
--
--         > $HOME/Library/Application Support/Rocket League/TAGame/Demos
--
--         For example:
--
--         > /Users/taylor/Library/Application Support/Rocket League/TAGame/Demos
--
--     [Linux]: [ ](#haddock-requires-something-here)
--
--         > $HOME/.local/share/Rocket League/TAGame/Demos
--
--         For example:
--
--         > /home/taylor/.local/share/Rocket League/TAGame/Demos
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
-- -   Decode a binary replay file and output a JSON replay file. This is by
--     far the most common way to use Endo.
--
--     > $ endo < example.replay > example.json
--     > $ endo -i example.replay -o example.json
--     > $ endo --mode decode --input example.replay --output example.json
--
-- -   Decode a binary replay file and output JSON replay to 'IO.stdout'. Note
--     that in the first example @--mode@ is optional because the default mode
--     is @decode@.
--
--     > $ endo < example.replay
--     > $ endo -i example.replay
--     > $ endo --mode decode --input example.replay
--
-- -   Encode a JSON replay file and output a binary replay file.
--
--     > $ endo -m encode < example.json > example.replay
--     > $ endo -i example.json -o example.replay
--     > $ endo --mode encode --input example.json --output example.replay
--
-- -   Encode a JSON replay file and output a binary replay to 'IO.stdout'.
--     Note that in the first example @--mode@ is required, otherwise it would
--     default to @decode@. Endo does not know the name of the file piped to
--     'IO.stdin'.
--
--     > $ endo -m encode < example.json
--     > $ endo -i example.json
--     > $ endo --mode encode --input example.json
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
replayFromBinary = runGet bytesToReplay

-- | Encodes a JSON replay. This is the opposite of 'replayFromJson'.
replayToJson :: Replay -> Bytes.ByteString
replayToJson =
  LazyBytes.toStrict . Aeson.encodingToLazyByteString . replayToJson_

-- | Decodes a JSON replay. This is the opposite of 'replayToJson'. Note that
-- converting from JSON and then back to JSON /is/ guaranteed to give you back
-- what you started with (unlike 'replayFromBinary').
replayFromJson :: Bytes.ByteString -> Either String Replay
replayFromJson bytes = do
  json <- Aeson.eitherDecodeStrict bytes
  Aeson.parseEither jsonToReplay json

-- | Encodes a binary replay. This is the opposite of 'replayFromBinary'.
replayToBinary :: Replay -> Bytes.ByteString
replayToBinary = runPut replayToBytes


-- | A Rocket League replay. Most of the information you'll usually want, like
-- the stuff shown on the scoreboard, is in the 'Header'. You'll typically only
-- need the 'Content' if you want to analyze the game data that's sent over the
-- network.
data Replay = Replay
  { replayHeader :: Section Header
  , replayContent :: Section Content
  }

bytesToReplay :: Binary.Get Replay
bytesToReplay =
  Replay <$> bytesToSection bytesToHeader <*> bytesToSection bytesToContent

replayToBytes :: Replay -> Binary.Put
replayToBytes replay =
  sectionToBytes headerToBytes (replayHeader replay)
    <> sectionToBytes contentToBytes (replayContent replay)

jsonToReplay :: Aeson.Value -> Aeson.Parser Replay
jsonToReplay = Aeson.withObject "Replay" $ \object ->
  Replay
    <$> requiredKey (jsonToSection jsonToHeader) object "header"
    <*> requiredKey (jsonToSection jsonToContent) object "content"

replayToJson_ :: Replay -> Aeson.Encoding
replayToJson_ replay =
  Aeson.pairs
    $ toPair (sectionToJson headerToJson) "header" (replayHeader replay)
    <> toPair (sectionToJson contentToJson) "content" (replayContent replay)


-- | A high-level section of a replay. Rocket League replays are split up into
-- two sections, each with a size and CRC. This type handles all of that for
-- you, so you shouldn't need to think about it.
newtype Section a
  = Section a

toSection :: a -> Section a
toSection = Section

fromSection :: Section a -> a
fromSection (Section a) = a

bytesToSection :: Binary.Get a -> Binary.Get (Section a)
bytesToSection decode = do
  size <- bytesToWord32
  expectedCrc <- bytesToWord32
  bytes <- Binary.getByteString $ word32ToInt size
  let actualCrc = crc32Bytes crc32Table crc32Initial bytes
  Monad.when (actualCrc /= expectedCrc)
    . fail
    $ "actual CRC "
    <> show actualCrc
    <> " does not match expected CRC "
    <> show expectedCrc
  toSection <$> either fail pure (runGet decode bytes)

sectionToBytes :: (a -> Binary.Put) -> Section a -> Binary.Put
sectionToBytes encode section =
  let bytes = runPut encode $ fromSection section
  in
    word32ToBytes (intToWord32 $ Bytes.length bytes)
    <> word32ToBytes (crc32Bytes crc32Table crc32Initial bytes)
    <> Binary.putByteString bytes

jsonToSection
  :: (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser (Section a)
jsonToSection decode = fmap toSection . decode

sectionToJson :: (a -> Aeson.Encoding) -> Section a -> Aeson.Encoding
sectionToJson encode = encode . fromSection


-- | The header or "meta" replay information. This includes everything that is
-- shown in the in-game replay menu and then some.
data Header = Header
  { headerMajorVersion :: Word.Word32
  -- ^ The major or "engine" version number. Note that this isn't the same as
  -- Rocket League's marketing version number, which is usually something like
  -- "v1.52". You may be able to convert between the two, but there's no
  -- guarantee that it's a one-to-one mapping.
  , headerMinorVersion :: Word.Word32
  -- ^ The minor or "licensee" version number.
  , headerPatchVersion :: Maybe Word.Word32
  -- ^ The patch or "net" version number. Replays before v1.35 (the
  -- "anniversary update") don't have this field.
  , headerLabel :: Text.Text
  -- ^ The label, which is always @\"TAGame.Replay_Soccar_TA\"@. This is most
  -- likely a [magic number](https://en.wikipedia.org/wiki/Magic_number_\(programming\))
  -- for the replay file format.
  , headerProperties :: HashMap.HashMap Text.Text Property
  -- ^ These properties determine how a replay will look in the list of replays
  -- in-game. One element is required for the replay to show up:
  --
  -- [@\"MapName\"@] This is a 'PropertyName' with a case-insensitive map
  -- identifier, like @\"Stadium_P\"@.
  --
  -- There are many other optional properties that affect how the replay looks
  -- in the list of replays:
  --
  -- [@\"Date\"@] A 'PropertyStr' with the format @"YYYY-mm-dd:HH-MM"@. Dates
  -- are not validated, but the month must be between 1 and 12 to show up. The
  -- hour is shown modulo 12 with AM or PM.
  --
  -- [@\"MatchType\"@] A 'PropertyName'. If this is not one of the expected
  -- values, nothing will be shown next to the replay's map. The expected
  -- values are: @\"Online\"@, @\"Offline\"@, @\"Private\"@, and @\"Season\"@.
  --
  -- [@\"NumFrames\"@] This 'PropertyInt' is used to calculate the length of
  -- the match. There are 30 frames per second, a typical 5-minute match has
  -- about 9,000 frames.
  --
  -- [@\"PrimaryPlayerTeam\"@] This is an 'PropertyInt'. It is either 0 (blue)
  -- or 1 (orange). Any other value is ignored. If this would be 0, you don't
  -- have to set it at all.
  --
  -- [@\"ReplayName\"@] An optional 'PropertyStr' with a user-supplied name for
  -- the replay.
  --
  -- [@\"Team0Score\"@] The blue team's score as an 'PropertyInt'. Can be
  -- omitted if the score is 0.
  --
  -- [@\"Team1Score\"@] The orange team's score as an 'PropertyInt'. Can also
  -- be omitted if the score is 0.
  --
  -- [@\"TeamSize\"@] An 'PropertyInt' with the number of players per team.
  -- This value is not validated, so you can put absurd values like 99.
  --
  -- [@\"bUnfairBots\"@] A 'PropertyBool' that makes "unfair" team sizes like
  -- 1v2.
  }

bytesToHeader :: Binary.Get Header
bytesToHeader = do
  majorVersion <- bytesToWord32
  minorVersion <- bytesToWord32
  Header majorVersion minorVersion
    <$> bytesToMaybe bytesToWord32 (hasPatchVersion majorVersion minorVersion)
    <*> bytesToText
    <*> bytesToHashMap bytesToProperty

headerToBytes :: Header -> Binary.Put
headerToBytes header =
  word32ToBytes (headerMajorVersion header)
    <> word32ToBytes (headerMinorVersion header)
    <> maybeToBytes word32ToBytes (headerPatchVersion header)
    <> textToBytes (headerLabel header)
    <> hashMapToBytes propertyToBytes (headerProperties header)

jsonToHeader :: Aeson.Value -> Aeson.Parser Header
jsonToHeader = Aeson.withObject "Header" $ \object ->
  Header
    <$> requiredKey jsonToWord32 object "majorVersion"
    <*> requiredKey jsonToWord32 object "minorVersion"
    <*> optionalKey jsonToWord32 object "patchVersion"
    <*> requiredKey jsonToText object "label"
    <*> requiredKey (jsonToHashMap jsonToProperty) object "properties"

headerToJson :: Header -> Aeson.Encoding
headerToJson header =
  Aeson.pairs
    $ toPair word32ToJson "majorVersion" (headerMajorVersion header)
    <> toPair word32ToJson "minorVersion" (headerMinorVersion header)
    <> toPair
         (maybeToJson word32ToJson)
         "patchVersion"
         (headerPatchVersion header)
    <> toPair textToJson "label" (headerLabel header)
    <> toPair
         (hashMapToJson propertyToJson)
         "properties"
         (headerProperties header)

hasPatchVersion :: Word.Word32 -> Word.Word32 -> Bool
hasPatchVersion majorVersion minorVersion =
  majorVersion >= 868 && minorVersion >= 18


-- | Properties are given in the header and usually describe high-level game
-- information like the player names and the information on their scoreboards.
data Property
  = PropertyArray (Vector.Vector (HashMap.HashMap Text.Text Property))
  | PropertyBool Bool
  | PropertyByte Text.Text Text.Text
  | PropertyFloat Float
  | PropertyInt Int.Int32
  | PropertyName Text.Text
  -- ^ Names are like strings except that they show up in the list of names in
  -- the content. It's not clear what this means exactly.
  | PropertyQWord Word.Word64
  | PropertyStr Text.Text

bytesToProperty :: Binary.Get Property
bytesToProperty = do
  kind <- bytesToText
  case kind of
    "ArrayProperty" -> do
      _size <- bytesToWord64 -- TODO
      PropertyArray <$> bytesToVector (bytesToHashMap bytesToProperty)
    "BoolProperty" -> do
      0 <- bytesToWord64
      PropertyBool <$> bytesToBool
    "ByteProperty" -> do
      size <- bytesToWord64
      key <- bytesToText
      if key == "OnlinePlatform_Steam"
        then pure $ PropertyByte "OnlinePlatform" key
        else PropertyByte key <$> Binary.isolate (word64ToInt size) bytesToText
    "FloatProperty" -> do
      4 <- bytesToWord64
      PropertyFloat <$> bytesToFloat
    "IntProperty" -> do
      4 <- bytesToWord64
      PropertyInt <$> bytesToInt32
    "NameProperty" -> do
      size <- bytesToWord64
      PropertyName <$> Binary.isolate (word64ToInt size) bytesToText
    "QWordProperty" -> do
      8 <- bytesToWord64
      PropertyQWord <$> bytesToWord64
    "StrProperty" -> do
      size <- bytesToWord64
      PropertyStr <$> Binary.isolate (word64ToInt size) bytesToText
    _ -> fail $ "unknown property kind: " <> show kind

propertyToBytes :: Property -> Binary.Put
propertyToBytes property = case property of
  PropertyArray vector ->
    textToBytes "ArrayProperty"
      <> word64ToBytes 0 -- TODO
      <> vectorToBytes (hashMapToBytes propertyToBytes) vector
  PropertyBool bool ->
    textToBytes "BoolProperty" <> word64ToBytes 0 <> boolToBytes bool
  PropertyByte key value ->
    let bytes = runPut textToBytes value
    in
      textToBytes "ByteProperty"
      <> word64ToBytes (intToWord64 $ Bytes.length bytes)
      <> textToBytes key
      <> textToBytes value
  PropertyFloat float ->
    textToBytes "FloatProperty" <> word64ToBytes 4 <> floatToBytes float
  PropertyInt int32 ->
    textToBytes "IntProperty" <> word64ToBytes 4 <> int32ToBytes int32
  PropertyName text ->
    let bytes = runPut textToBytes text
    in
      textToBytes "NameProperty"
      <> word64ToBytes (intToWord64 $ Bytes.length bytes)
      <> Binary.putByteString bytes
  PropertyQWord word64 ->
    textToBytes "QWordProperty" <> word64ToBytes 8 <> word64ToBytes word64
  PropertyStr text ->
    let bytes = runPut textToBytes text
    in
      textToBytes "StrProperty"
      <> word64ToBytes (intToWord64 $ Bytes.length bytes)
      <> Binary.putByteString bytes

jsonToProperty :: Aeson.Value -> Aeson.Parser Property
jsonToProperty = Aeson.withObject "Property" $ \object -> do
  kind <- requiredKey jsonToText object "kind"
  case kind of
    "array" -> PropertyArray <$> requiredKey
      (jsonToVector $ jsonToHashMap jsonToProperty)
      object
      "value"
    "bool" -> PropertyBool <$> requiredKey jsonToBool object "value"
    "byte" ->
      PropertyByte <$> requiredKey jsonToText object "key" <*> requiredKey
        jsonToText
        object
        "value"
    "float" -> PropertyFloat <$> requiredKey jsonToFloat object "value"
    "int" -> PropertyInt <$> requiredKey jsonToInt32 object "value"
    "name" -> PropertyName <$> requiredKey jsonToText object "value"
    "qWord" -> PropertyQWord <$> requiredKey jsonToWord64 object "value"
    "str" -> PropertyStr <$> requiredKey jsonToText object "value"
    _ -> fail $ "unknown property kind: " <> show kind

propertyToJson :: Property -> Aeson.Encoding
propertyToJson property = Aeson.pairs $ case property of
  PropertyArray vector ->
    toPair textToJson "kind" "array"
      <> toPair (vectorToJson $ hashMapToJson propertyToJson) "value" vector
  PropertyBool bool ->
    toPair textToJson "kind" "bool" <> toPair boolToJson "value" bool
  PropertyByte key value ->
    toPair textToJson "kind" "byte"
      <> toPair textToJson "key" key
      <> toPair textToJson "value" value
  PropertyFloat float ->
    toPair textToJson "kind" "float" <> toPair floatToJson "value" float
  PropertyInt int32 ->
    toPair textToJson "kind" "int" <> toPair int32ToJson "value" int32
  PropertyName text ->
    toPair textToJson "kind" "name" <> toPair textToJson "value" text
  PropertyQWord word64 ->
    toPair textToJson "kind" "qWord" <> toPair word64ToJson "value" word64
  PropertyStr text ->
    toPair textToJson "kind" "str" <> toPair textToJson "value" text


-- | The content or "data" replay information. This includes everything that is
-- shown when watching a replay in game.
data Content = Content
  { contentLevels :: Vector.Vector Text.Text
  -- ^ It's not entirely clear what this represents. Typically there is only
  -- one level, like @"stadium_oob_audio_map"@.
  , contentKeyFrames :: Vector.Vector KeyFrame
  -- ^ A list of which frames are key frames. Although they aren't necessary
  -- for replays, key frames are frames that replicate every actor. They
  -- typically happen once every 10 seconds.
  , contentFrames :: Frames
  -- ^ The actual game data. This is where all the interesting information is.
  -- If you can see it by watching a replay in the Rocket League client, it's
  -- in here.
  , contentMessages :: Vector.Vector Message
  -- ^ Debug messages. These only exist in very old replays.
  , contentMarks :: Vector.Vector Mark
  -- ^ Ticks marks shown on the scrubber when watching a replay.
  , contentPackages :: Vector.Vector Text.Text
  -- ^ A list of @.upk@ files to load, like
  -- @"..\..\TAGame\CookedPCConsole\Stadium_P.upk"@.
  , contentObjects :: Vector.Vector Text.Text
  -- ^ Objects in the stream.
  , contentNames :: Vector.Vector Text.Text
  -- ^ It's not clear what these are used for. This list is usually not empty,
  -- but appears unused otherwise.
  , contentClassMappings :: Vector.Vector ClassMapping
  -- ^ A mapping between classes and their ID in the stream.
  , contentCaches :: Vector.Vector Cache
  -- ^ A list of classes along with their parent classes and attributes.
  }

bytesToContent :: Binary.Get Content
bytesToContent =
  Content
    <$> bytesToVector bytesToText
    <*> bytesToVector bytesToKeyFrame
    <*> bytesToFrames
    <*> bytesToVector bytesToMessage
    <*> bytesToVector bytesToMark
    <*> bytesToVector bytesToText
    <*> bytesToVector bytesToText
    <*> bytesToVector bytesToText
    <*> bytesToVector bytesToClassMapping
    <*> bytesToVector bytesToCache

contentToBytes :: Content -> Binary.Put
contentToBytes content =
  vectorToBytes textToBytes (contentLevels content)
    <> vectorToBytes keyFrameToBytes (contentKeyFrames content)
    <> framesToBytes (contentFrames content)
    <> vectorToBytes messageToBytes (contentMessages content)
    <> vectorToBytes markToBytes (contentMarks content)
    <> vectorToBytes textToBytes (contentPackages content)
    <> vectorToBytes textToBytes (contentObjects content)
    <> vectorToBytes textToBytes (contentNames content)
    <> vectorToBytes classMappingToBytes (contentClassMappings content)
    <> vectorToBytes cacheToBytes (contentCaches content)

jsonToContent :: Aeson.Value -> Aeson.Parser Content
jsonToContent = Aeson.withObject "Content" $ \object ->
  Content
    <$> requiredKey (jsonToVector jsonToText) object "levels"
    <*> requiredKey (jsonToVector jsonToKeyFrame) object "keyFrames"
    <*> requiredKey jsonToFrames object "frames"
    <*> requiredKey (jsonToVector jsonToMessage) object "messages"
    <*> requiredKey (jsonToVector jsonToMark) object "marks"
    <*> requiredKey (jsonToVector jsonToText) object "packages"
    <*> requiredKey (jsonToVector jsonToText) object "objects"
    <*> requiredKey (jsonToVector jsonToText) object "names"
    <*> requiredKey (jsonToVector jsonToClassMapping) object "classMappings"
    <*> requiredKey (jsonToVector jsonToCache) object "caches"

contentToJson :: Content -> Aeson.Encoding
contentToJson content =
  Aeson.pairs
    $ toPair (vectorToJson textToJson) "levels" (contentLevels content)
    <> toPair
         (vectorToJson keyFrameToJson)
         "keyFrames"
         (contentKeyFrames content)
    <> toPair framesToJson "frames" (contentFrames content)
    <> toPair (vectorToJson messageToJson) "messages" (contentMessages content)
    <> toPair (vectorToJson markToJson) "marks" (contentMarks content)
    <> toPair (vectorToJson textToJson) "packages" (contentPackages content)
    <> toPair (vectorToJson textToJson) "objects" (contentObjects content)
    <> toPair (vectorToJson textToJson) "names" (contentNames content)
    <> toPair
         (vectorToJson classMappingToJson)
         "classMappings"
         (contentClassMappings content)
    <> toPair (vectorToJson cacheToJson) "caches" (contentCaches content)


-- | Meta information about a frame that replicates every actor. Key frames are
-- useful for quickly scrubbing through replays.
data KeyFrame = KeyFrame
  { keyFrameTime :: Float
  -- ^ The number of seconds since the beginning on the replay.
  , keyFrameFrame :: Word.Word32
  -- ^ The frame number of this key frame, starting from 0 at the beginning of
  -- the replay.
  , keyFramePosition :: Word.Word32
  -- ^ The bit position of this key frame in the network stream.
  }

bytesToKeyFrame :: Binary.Get KeyFrame
bytesToKeyFrame =
  KeyFrame <$> bytesToFloat <*> bytesToWord32 <*> bytesToWord32

keyFrameToBytes :: KeyFrame -> Binary.Put
keyFrameToBytes keyFrame =
  floatToBytes (keyFrameTime keyFrame)
    <> word32ToBytes (keyFrameFrame keyFrame)
    <> word32ToBytes (keyFramePosition keyFrame)

jsonToKeyFrame :: Aeson.Value -> Aeson.Parser KeyFrame
jsonToKeyFrame = Aeson.withObject "KeyFrame" $ \object ->
  KeyFrame
    <$> requiredKey jsonToFloat object "time"
    <*> requiredKey jsonToWord32 object "frame"
    <*> requiredKey jsonToWord32 object "position"

keyFrameToJson :: KeyFrame -> Aeson.Encoding
keyFrameToJson keyFrame =
  Aeson.pairs
    $ toPair floatToJson "time" (keyFrameTime keyFrame)
    <> toPair word32ToJson "frame" (keyFrameFrame keyFrame)
    <> toPair word32ToJson "position" (keyFramePosition keyFrame)


-- TODO
-- | This is a placeholder until the frames can actually be handled.
newtype Frames
  = Frames Bytes.ByteString

toFrames :: Bytes.ByteString -> Frames
toFrames = Frames

fromFrames :: Frames -> Bytes.ByteString
fromFrames (Frames bytes) = bytes

bytesToFrames :: Binary.Get Frames
bytesToFrames = do
  size <- bytesToWord32
  toFrames <$> Binary.getByteString (word32ToInt size)

framesToBytes :: Frames -> Binary.Put
framesToBytes frames =
  let bytes = fromFrames frames
  in
    word32ToBytes (intToWord32 $ Bytes.length bytes)
      <> Binary.putByteString bytes

jsonToFrames :: Aeson.Value -> Aeson.Parser Frames
jsonToFrames =
  Aeson.withText "Frames"
    $ either fail (pure . toFrames)
    . Base64.decode
    . Text.encodeUtf8

framesToJson :: Frames -> Aeson.Encoding
framesToJson = Aeson.toEncoding . Text.decodeUtf8 . Base64.encode . fromFrames


-- | A debug message.
data Message = Message
  { messageFrame :: Word.Word32
  -- ^ Which frame this message belongs to.
  , messageName :: Text.Text
  -- ^ The primary player's name.
  , messageValue :: Text.Text
  -- ^ The actual payload of the message.
  }

bytesToMessage :: Binary.Get Message
bytesToMessage = Message <$> bytesToWord32 <*> bytesToText <*> bytesToText

messageToBytes :: Message -> Binary.Put
messageToBytes message =
  word32ToBytes (messageFrame message)
    <> textToBytes (messageName message)
    <> textToBytes (messageValue message)

jsonToMessage :: Aeson.Value -> Aeson.Parser Message
jsonToMessage = Aeson.withObject "Message" $ \object ->
  Message
    <$> requiredKey jsonToWord32 object "frame"
    <*> requiredKey jsonToText object "name"
    <*> requiredKey jsonToText object "value"

messageToJson :: Message -> Aeson.Encoding
messageToJson message =
  Aeson.pairs
    $ toPair word32ToJson "frame" (messageFrame message)
    <> toPair textToJson "name" (messageName message)
    <> toPair textToJson "value" (messageValue message)


-- | A tick mark on the replay scrubber.
data Mark = Mark
  { markValue :: Text.Text
  -- ^ Which type of mark this is, like @\"Team0Goal\"@.
  , markFrame :: Word.Word32
  -- ^ Which frame this mark belongs to, starting from 0.
  }

bytesToMark :: Binary.Get Mark
bytesToMark = Mark <$> bytesToText <*> bytesToWord32

markToBytes :: Mark -> Binary.Put
markToBytes mark =
  textToBytes (markValue mark) <> word32ToBytes (markFrame mark)

jsonToMark :: Aeson.Value -> Aeson.Parser Mark
jsonToMark = Aeson.withObject "Mark" $ \object ->
  Mark <$> requiredKey jsonToText object "value" <*> requiredKey
    jsonToWord32
    object
    "frame"

markToJson :: Mark -> Aeson.Encoding
markToJson mark =
  Aeson.pairs $ toPair textToJson "value" (markValue mark) <> toPair
    word32ToJson
    "frame"
    (markFrame mark)


data ClassMapping = ClassMapping
  { classMappingName :: Text.Text
  , classMappingStreamId :: Word.Word32
  }

bytesToClassMapping :: Binary.Get ClassMapping
bytesToClassMapping = ClassMapping <$> bytesToText <*> bytesToWord32

classMappingToBytes :: ClassMapping -> Binary.Put
classMappingToBytes classMapping = textToBytes (classMappingName classMapping)
  <> word32ToBytes (classMappingStreamId classMapping)

jsonToClassMapping :: Aeson.Value -> Aeson.Parser ClassMapping
jsonToClassMapping = Aeson.withObject "ClassMapping" $ \object ->
  ClassMapping <$> requiredKey jsonToText object "name" <*> requiredKey
    jsonToWord32
    object
    "streamId"

classMappingToJson :: ClassMapping -> Aeson.Encoding
classMappingToJson classMapping =
  Aeson.pairs
    $ toPair textToJson "name" (classMappingName classMapping)
    <> toPair word32ToJson "streamId" (classMappingStreamId classMapping)


data Cache = Cache
  { cacheClassId :: Word.Word32
  , cacheParentCacheId :: Word.Word32
  , cacheCacheId :: Word.Word32
  , cacheAttributeMappings :: Vector.Vector AttributeMapping
  }

bytesToCache :: Binary.Get Cache
bytesToCache =
  Cache
    <$> bytesToWord32
    <*> bytesToWord32
    <*> bytesToWord32
    <*> bytesToVector bytesToAttributeMapping

cacheToBytes :: Cache -> Binary.Put
cacheToBytes cache =
  word32ToBytes (cacheClassId cache)
    <> word32ToBytes (cacheParentCacheId cache)
    <> word32ToBytes (cacheCacheId cache)
    <> vectorToBytes attributeMappingToBytes (cacheAttributeMappings cache)

jsonToCache :: Aeson.Value -> Aeson.Parser Cache
jsonToCache = Aeson.withObject "Cache" $ \object ->
  Cache
    <$> requiredKey jsonToWord32 object "classId"
    <*> requiredKey jsonToWord32 object "parentCacheId"
    <*> requiredKey jsonToWord32 object "cacheId"
    <*> requiredKey
          (jsonToVector jsonToAttributeMapping)
          object
          "attributeMappings"

cacheToJson :: Cache -> Aeson.Encoding
cacheToJson cache =
  Aeson.pairs
    $ toPair word32ToJson "classId" (cacheClassId cache)
    <> toPair word32ToJson "parentCacheId" (cacheParentCacheId cache)
    <> toPair word32ToJson "cacheId" (cacheCacheId cache)
    <> toPair
         (vectorToJson attributeMappingToJson)
         "attributeMappings"
         (cacheAttributeMappings cache)


data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word.Word32
  , attributeMappingStreamId :: Word.Word32
  }

bytesToAttributeMapping :: Binary.Get AttributeMapping
bytesToAttributeMapping = AttributeMapping <$> bytesToWord32 <*> bytesToWord32

attributeMappingToBytes :: AttributeMapping -> Binary.Put
attributeMappingToBytes attributeMapping =
  word32ToBytes (attributeMappingObjectId attributeMapping)
    <> word32ToBytes (attributeMappingStreamId attributeMapping)

jsonToAttributeMapping :: Aeson.Value -> Aeson.Parser AttributeMapping
jsonToAttributeMapping = Aeson.withObject "AttributeMapping" $ \object ->
  AttributeMapping
    <$> requiredKey jsonToWord32 object "objectId"
    <*> requiredKey jsonToWord32 object "streamId"

attributeMappingToJson :: AttributeMapping -> Aeson.Encoding
attributeMappingToJson attributeMapping =
  Aeson.pairs
    $ toPair
        word32ToJson
        "objectId"
        (attributeMappingObjectId attributeMapping)
    <> toPair
         word32ToJson
         "streamId"
         (attributeMappingStreamId attributeMapping)


bytesToBool :: Binary.Get Bool
bytesToBool = do
  byte <- Binary.getWord8
  case byte of
    0 -> pure False
    1 -> pure True
    _ -> fail $ "invalid boolean: " <> show byte

boolToBytes :: Bool -> Binary.Put
boolToBytes = Binary.putWord8 . Bool.bool 0 1

jsonToBool :: Aeson.Value -> Aeson.Parser Bool
jsonToBool = Aeson.parseJSON

boolToJson :: Bool -> Aeson.Encoding
boolToJson = Aeson.toEncoding


bytesToFloat :: Binary.Get Float
bytesToFloat = word32ToFloat <$> bytesToWord32

floatToBytes :: Float -> Binary.Put
floatToBytes = word32ToBytes . floatToWord32

jsonToFloat :: Aeson.Value -> Aeson.Parser Float
jsonToFloat = Aeson.parseJSON

floatToJson :: Float -> Aeson.Encoding
floatToJson = Aeson.toEncoding


bytesToHashMap :: Binary.Get v -> Binary.Get (HashMap.HashMap Text.Text v)
bytesToHashMap decode = bytesToHashMapWith decode HashMap.empty

bytesToHashMapWith
  :: Binary.Get v
  -> HashMap.HashMap Text.Text v
  -> Binary.Get (HashMap.HashMap Text.Text v)
bytesToHashMapWith decode hashMap = do
  key <- bytesToText
  if key == "None"
    then pure hashMap
    else do
      value <- decode
      bytesToHashMapWith decode $ HashMap.insert key value hashMap

hashMapToBytes
  :: (v -> Binary.Put) -> HashMap.HashMap Text.Text v -> Binary.Put
hashMapToBytes encode = HashMap.foldrWithKey
  (\key value put -> textToBytes key <> encode value <> put)
  (textToBytes "None")

jsonToHashMap
  :: (Aeson.Value -> Aeson.Parser v)
  -> Aeson.Value
  -> Aeson.Parser (HashMap.HashMap Text.Text v)
jsonToHashMap = Aeson.withObject "HashMap" . mapM

hashMapToJson
  :: (v -> Aeson.Encoding) -> HashMap.HashMap Text.Text v -> Aeson.Encoding
hashMapToJson encode = Aeson.dict Aeson.text encode HashMap.foldrWithKey


bytesToInt32 :: Binary.Get Int.Int32
bytesToInt32 = Binary.getInt32le

int32ToBytes :: Int.Int32 -> Binary.Put
int32ToBytes = Binary.putInt32le

jsonToInt32 :: Aeson.Value -> Aeson.Parser Int.Int32
jsonToInt32 = Aeson.parseJSON

int32ToJson :: Int.Int32 -> Aeson.Encoding
int32ToJson = Aeson.toEncoding


bytesToMaybe :: Binary.Get a -> Bool -> Binary.Get (Maybe a)
bytesToMaybe decode condition =
  if condition then Just <$> decode else pure Nothing

maybeToBytes :: (a -> Binary.Put) -> Maybe a -> Binary.Put
maybeToBytes = maybe mempty

jsonToMaybe
  :: (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser (Maybe a)
jsonToMaybe decode json = case json of
  Aeson.Null -> pure Nothing
  _ -> Just <$> decode json

maybeToJson :: (a -> Aeson.Encoding) -> Maybe a -> Aeson.Encoding
maybeToJson = maybe Aeson.null_


bytesToWord32 :: Binary.Get Word.Word32
bytesToWord32 = Binary.getWord32le

word32ToBytes :: Word.Word32 -> Binary.Put
word32ToBytes = Binary.putWord32le

jsonToWord32 :: Aeson.Value -> Aeson.Parser Word.Word32
jsonToWord32 = Aeson.parseJSON

word32ToJson :: Word.Word32 -> Aeson.Encoding
word32ToJson = Aeson.toEncoding


bytesToWord64 :: Binary.Get Word.Word64
bytesToWord64 = Binary.getWord64le

word64ToBytes :: Word.Word64 -> Binary.Put
word64ToBytes = Binary.putWord64le

jsonToWord64 :: Aeson.Value -> Aeson.Parser Word.Word64
jsonToWord64 = Aeson.parseJSON

word64ToJson :: Word.Word64 -> Aeson.Encoding
word64ToJson = Aeson.toEncoding


bytesToText :: Binary.Get Text.Text
bytesToText = do
  rawSize <- bytesToInt32
  let size = if rawSize == 0x05000000 then 8 else rawSize
  Text.filter (/= '\x00') <$> if size < 0
    then Text.decodeUtf16LE <$> Binary.getByteString (-2 * int32ToInt size)
    else Text.decodeLatin1 <$> Binary.getByteString (int32ToInt size)

textToBytes :: Text.Text -> Binary.Put
textToBytes text
  = let textWithNull = Text.snoc text '\x00'
    in
      if Text.all Char.isLatin1 textWithNull
        then
          let bytes = encodeLatin1 textWithNull
          in
            int32ToBytes (intToInt32 $ Bytes.length bytes)
              <> Binary.putByteString bytes
        else
          let bytes = Text.encodeUtf16LE textWithNull
          in
            int32ToBytes
                (flip div 2 . negate . intToInt32 $ Bytes.length bytes)
              <> Binary.putByteString bytes

jsonToText :: Aeson.Value -> Aeson.Parser Text.Text
jsonToText = Aeson.parseJSON

textToJson :: Text.Text -> Aeson.Encoding
textToJson = Aeson.toEncoding


bytesToVector :: Binary.Get a -> Binary.Get (Vector.Vector a)
bytesToVector decode = do
  size <- bytesToWord32
  Vector.replicateM (word32ToInt size) decode

vectorToBytes :: (a -> Binary.Put) -> Vector.Vector a -> Binary.Put
vectorToBytes encode vector = do
  word32ToBytes . intToWord32 $ Vector.length vector
  Vector.mapM_ encode vector

jsonToVector
  :: (Aeson.Value -> Aeson.Parser a)
  -> Aeson.Value
  -> Aeson.Parser (Vector.Vector a)
jsonToVector = Aeson.withArray "Vector" . mapM

vectorToJson :: (a -> Aeson.Encoding) -> Vector.Vector a -> Aeson.Encoding
vectorToJson encode = Aeson.list encode . Vector.toList


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

encodeLatin1 :: Text.Text -> Bytes.ByteString
encodeLatin1 = Latin1.pack . Text.unpack

floatToWord32 :: Float -> Word.Word32
floatToWord32 = Unsafe.unsafeCoerce

hasExtension :: String -> FilePath -> Bool
hasExtension = List.isSuffixOf

int32ToInt :: Int.Int32 -> Int
int32ToInt = fromIntegral

intToInt32 :: Int -> Int.Int32
intToInt32 = fromIntegral

intToWord32 :: Int -> Word.Word32
intToWord32 = fromIntegral

intToWord64 :: Int -> Word.Word64
intToWord64 = fromIntegral

optionalKey
  :: (Aeson.Value -> Aeson.Parser v)
  -> Aeson.Object
  -> Text.Text
  -> Aeson.Parser (Maybe v)
optionalKey decode object key = do
  value <- object Aeson..:? key
  maybe (pure Nothing) (jsonToMaybe decode) value

requiredKey
  :: (Aeson.Value -> Aeson.Parser v)
  -> Aeson.Object
  -> Text.Text
  -> Aeson.Parser v
requiredKey decode object key = do
  json <- object Aeson..: key
  decode json

runGet :: Binary.Get a -> Bytes.ByteString -> Either String a
runGet decode =
  either (Left . third) (Right . third)
    . Binary.runGetOrFail decode
    . LazyBytes.fromStrict

runPut :: (a -> Binary.Put) -> a -> Bytes.ByteString
runPut encode = LazyBytes.toStrict . Binary.runPut . encode

third :: (a, b, c) -> c
third (_, _, c) = c

toPair :: (v -> Aeson.Encoding) -> Text.Text -> v -> Aeson.Series
toPair encode key = Aeson.pair key . encode

warnLn :: String -> IO ()
warnLn = IO.hPutStrLn IO.stderr

warn :: String -> IO ()
warn = IO.hPutStr IO.stderr

word32ToFloat :: Word.Word32 -> Float
word32ToFloat = Unsafe.unsafeCoerce

word8ToInt :: Word.Word8 -> Int
word8ToInt = fromIntegral

word32ToInt :: Word.Word32 -> Int
word32ToInt = fromIntegral

word32ToWord8 :: Word.Word32 -> Word.Word8
word32ToWord8 = fromIntegral

word64ToInt :: Word.Word64 -> Int
word64ToInt = fromIntegral
