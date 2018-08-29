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
    originalFile =
      FilePath.combine "replays" (FilePath.addExtension name "replay")
  original <- Bytes.readFile originalFile
  let originalSize = Bytes.length original
  Printf.printf "  original size: %d bytes\n" originalSize

  ((jsonFile, decodeCount), decodeTime) <- withTime
    (withAllocationCount (decode directory name originalFile))
  Printf.printf
    "  decoding allocated: %d bytes (%dx)\n"
    decodeCount
    (div decodeCount (intToInt64 originalSize))
  Printf.printf
    "  decoding elapsed: %d nanoseconds (%.3f MBps)\n"
    (Clock.toNanoSecs decodeTime)
    (mbps originalSize decodeTime)

  json <- Bytes.readFile jsonFile
  Printf.printf "  JSON size: %d bytes\n" (Bytes.length json)

  ((modifiedFile, encodeCount), encodeTime) <- withTime
    (withAllocationCount (encode directory name jsonFile))
  Printf.printf
    "  encoding allocated: %d bytes (%dx)\n"
    encodeCount
    (div encodeCount (intToInt64 originalSize))
  Printf.printf
    "  encoding elapsed: %d nanoseconds (%.3f MBps)\n"
    (Clock.toNanoSecs encodeTime)
    (mbps originalSize encodeTime)

  modified <- Bytes.readFile modifiedFile
  Printf.printf "  modified size: %d bytes\n" (Bytes.length modified)

  if modified == original
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
