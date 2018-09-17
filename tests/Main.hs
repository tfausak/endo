module Main
  ( main
  )
where

import qualified Control.Monad as Monad
import qualified Data.ByteString as Bytes
import qualified Data.Int as Int
import qualified Endo
import qualified System.Clock as Clock
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Mem as Mem
import qualified Text.Printf as Printf


replays :: [(String, String)]
replays =
  [ ("0008", "a flip time")
  , ("000b", "nintendo switch")
  , ("07e9", "a game mode before Neo Tokyo")
  , ("0ad2", "some Latin-1 text")
  , ("1205", "rumble mode")
  , ("160c", "a dedicated server IP")
  , ("16d5", "new property types")
  , ("18d6", "an online loadout attribute")
  , ("1a12", "overtime")
  , ("1ae4", "a game time")
  , ("1bc2", "no padding after the frames")
  , ("1d1d", "a camera pitch")
  , ("1ef9", "a private hoops match")
  , ("1f37", "splitscreen players")
  , ("2114", "a match save")
  , ("2266", "dropshot")
  , ("22ba", "a vote to forfeit")
  , ("27b6", "some UTF-16 text")
  , ("29f5", "frames")
  , ("2cfe", "a new playstation id")
  , ("3381", "patch 1.37")
  , ("372d", "a camera yaw attribute")
  , ("387f", "a frozen attribute")
  , ("3abd", "rlcs")
  , ("3ea1", "a custom team name")
  , ("4126", "a game mode after Neo Tokyo")
  , ("419a", "a club match")
  , ("42f0", "reservations after Neo Tokyo")
  , ("504e", "some messages")
  , ("520e", "no pickup attribute")
  , ("524f", "quat edge case")
  , ("52aa", "a match-ending attribute")
  , ("540d", "a demolish attribute")
  , ("551c", "private match settings")
  , ("6210", "different player history key")
  , ("6320", "a forfeit attribute")
  , ("6688", "a malformed byte property")
  , ("6b0d", "patch 1.37")
  , ("6d1b", "a flip right")
  , ("6f7c", "a map with numbers")
  , ("7083", "weird basketball capitalization")
  , ("7109", "a boost modifier")
  , ("75ce", "primary and secondary titles")
  , ("7bf6", "an online loadouts attribute")
  , ("89cb", "remote user data")
  , ("8ae5", "new painted items")
  , ("92a6", "with server performance state")
  , ("946f", "patch 1.43")
  , ("9704", "a batarang")
  , ("98e5", "a player using behind view")
  , ("a09e", "a tournament")
  , ("a128", "a round count down")
  , ("a52f", "some more mutators")
  , ("a558", "extended explosion data")
  , ("a671", "a waiting player")
  , ("a676", "new user color")
  , ("a7f0", "a ready attribute")
  , ("a9df", "salty shores patch 1.45")
  , ("aa70", "patch 1.50 - TitleID attribute")
  , ("afb1", "patch 1.37")
  , ("b9f9", "a party leader")
  , ("c14f", "some mutators")
  , ("c837", "a spectator")
  , ("cc4c", "after Starbase ARC")
  , ("d044", "hoops mutators")
  , ("d236", "rlcs s2")
  , ("d428", "a private hockey match")
  , ("d7fb", "an explosion attribute")
  , ("db70", "new lag indicator")
  , ("dcb3", "a pawn type attribute")
  , ("de56", "a problematic product attribute")
  , ("e80d", "unlimited time")
  , ("eae3", "an actor/object ID collision")
  , ("eae8", "custom team colors")
  , ("f299", "a location attribute")
  , ("f7b9", "a hockey game event")
  , ("f811", "no frames")
  , ("fdc7", "an MVP")
  ]


main :: IO ()
main = Temp.withSystemTempDirectory "endo-" mainWith

mainWith :: FilePath -> IO ()
mainWith directory = do
  IO.hPutStr IO.stderr $ unlines
    [ "Columns:"
    , "1.  Replay ID"
    , "2.  Binary file size in bytes"
    , "3.  Bytes allocated while decoding"
    , "4.  #3 / #2: Decoding allocation related to binary file size"
    , "5.  Nanoseconds elapsed while decoding"
    , "6.  #2 / #5: Decoding rate in mebibytes per second"
    , "7.  JSON file size in bytes"
    , "8.  #7 / #2: JSON file size related to binary file size"
    , "9.  Bytes allocated while encoding"
    , "10. #9 / #2: Encoding allocation related to binary file size"
    , "11. Nanoseconds elapsed while encoding"
    , "12. #2 / #11: Encoding rate in mebibytes per second"
    ]
  mapM_ (test directory) replays

test :: FilePath -> (String, String) -> IO ()
test directory (name, description) = do
  let
    originalReplayFile =
      FilePath.combine "replays" (FilePath.addExtension name "replay")
  originalReplay <- Bytes.readFile originalReplayFile
  let originalReplaySize = Bytes.length originalReplay
  ((originalJsonFile, decodeCount), decodeTime) <- withTime
    (withAllocationCount (decode directory name originalReplayFile))
  originalJson <- Bytes.readFile originalJsonFile
  let originalJsonSize = Bytes.length originalJson
  ((modifiedReplayFile, encodeCount), encodeTime) <- withTime
    (withAllocationCount (encode directory name originalJsonFile))
  modifiedJsonFile <- decode directory name modifiedReplayFile
  modifiedJson <- Bytes.readFile modifiedJsonFile

  Monad.unless (modifiedJson == originalJson) . Exit.die $ Printf.printf
    "%s: %s"
    name
    description

  Printf.printf
    "%4s\t%7d\t%8d\t%3d\t%8d\t%4.1f\t%7d\t%5.3f\t%8d\t%3d\t%8d\t%4.1f\n"
    name
    originalReplaySize
    decodeCount
    (div decodeCount (intToInt64 originalReplaySize))
    (Clock.toNanoSecs decodeTime)
    (mbps originalReplaySize decodeTime)
    originalJsonSize
    (fromIntegral originalJsonSize / fromIntegral originalReplaySize :: Float)
    encodeCount
    (div encodeCount (intToInt64 originalReplaySize))
    (Clock.toNanoSecs encodeTime)
    (mbps originalReplaySize encodeTime)

mbps :: Int -> Clock.TimeSpec -> Double
mbps bytes elapsed = bytesToMegabytes (intToDouble bytes)
  / nanosecondsToSeconds (integerToDouble (Clock.toNanoSecs elapsed))

bytesToMegabytes :: Double -> Double
bytesToMegabytes = (/ (1024 * 1024))

nanosecondsToSeconds :: Double -> Double
nanosecondsToSeconds = (/ 1000000000)

decode :: FilePath -> String -> FilePath -> IO FilePath
decode directory replay input = do
  let
    output = FilePath.combine directory (FilePath.addExtension replay "json")
  endo ["--input", input, "--output", output]
  pure output

encode :: FilePath -> String -> FilePath -> IO FilePath
encode directory replay input = do
  let
    output =
      FilePath.combine directory (FilePath.addExtension replay "replay")
  endo ["--input", input, "--output", output]
  pure output

endo :: [String] -> IO ()
endo = Endo.mainWith "endo"

withAllocationCount :: IO a -> IO (a, Int.Int64)
withAllocationCount action = do
  before <- Mem.getAllocationCounter
  result <- action
  after <- Mem.getAllocationCounter
  pure (result, before - after)

withTime :: IO a -> IO (a, Clock.TimeSpec)
withTime action = do
  before <- Clock.getTime Clock.Monotonic
  result <- action
  after <- Clock.getTime Clock.Monotonic
  pure (result, Clock.diffTimeSpec before after)

intToInt64 :: Int -> Int.Int64
intToInt64 = fromIntegral

intToDouble :: Int -> Double
intToDouble = fromIntegral

integerToDouble :: Integer -> Double
integerToDouble = fromIntegral
