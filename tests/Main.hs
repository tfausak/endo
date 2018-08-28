module Main
  ( main
  )
where

import qualified Control.Exception as Exception
import qualified Data.Int as Int
import qualified Endo
import qualified System.Exit as Exit
import qualified System.Mem as Mem


main :: IO ()
main = do
  ((), count) <- withAllocationCount
    (withoutExit (Endo.mainWith "test" ["--help"]))
  putStrLn ("allocated " <> show count <> " bytes")

withoutExit :: IO () -> IO ()
withoutExit = Exception.handle ignoreExit

ignoreExit :: Exit.ExitCode -> IO ()
ignoreExit _ = pure ()

withAllocationCount :: IO a -> IO (a, Int.Int64)
withAllocationCount action = do
  before <- Mem.getAllocationCounter
  result <- action
  after <- Mem.getAllocationCounter
  pure (result, before - after)
