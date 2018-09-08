module Main
  ( main
  )
where

import qualified Data.ByteString as Bytes
import qualified Data.Int as Int
import qualified Endo
import qualified System.Clock as Clock
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import qualified System.Mem as Mem
import qualified Text.Printf as Printf


replays :: [(String, String)]
replays = [("f811", "without any frames")]


main :: IO ()
main = Temp.withSystemTempDirectory "endo-" mainWith

mainWith :: FilePath -> IO ()
mainWith directory = mapM_ (test directory) replays

test :: FilePath -> (String, String) -> IO ()
test directory (name, description) = do
  Printf.printf "- %s: %s\n" name description

  let
    originalReplayFile =
      FilePath.combine "replays" (FilePath.addExtension name "replay")
  originalReplay <- Bytes.readFile originalReplayFile
  let originalReplaySize = Bytes.length originalReplay
  Printf.printf "  original size: %d bytes\n" originalReplaySize

  ((originalJsonFile, decodeCount), decodeTime) <- withTime
    (withAllocationCount (decode directory name originalReplayFile))
  Printf.printf
    "  decoding allocated: %d bytes (%dx)\n"
    decodeCount
    (div decodeCount (intToInt64 originalReplaySize))
  Printf.printf
    "  decoding elapsed: %d nanoseconds (%.3f MBps)\n"
    (Clock.toNanoSecs decodeTime)
    (mbps originalReplaySize decodeTime)

  originalJson <- Bytes.readFile originalJsonFile
  let originalJsonSize = Bytes.length originalJson
  Printf.printf
    "  JSON size: %d bytes (%.3fx)\n"
    originalJsonSize
    (fromIntegral originalJsonSize / fromIntegral originalReplaySize :: Float)

  ((modifiedReplayFile, encodeCount), encodeTime) <- withTime
    (withAllocationCount (encode directory name originalJsonFile))
  Printf.printf
    "  encoding allocated: %d bytes (%dx)\n"
    encodeCount
    (div encodeCount (intToInt64 originalReplaySize))
  Printf.printf
    "  encoding elapsed: %d nanoseconds (%.3f MBps)\n"
    (Clock.toNanoSecs encodeTime)
    (mbps originalReplaySize encodeTime)

  modifiedJsonFile <- decode directory name modifiedReplayFile
  modifiedJson <- Bytes.readFile modifiedJsonFile
  if modifiedJson == originalJson
    then putStrLn "  round tripping succeeded"
    else Exit.die "  round tripping failed"

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
