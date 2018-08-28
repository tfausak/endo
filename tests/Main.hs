module Main
  ( main
  )
where

import qualified Endo

main :: IO ()
main = Endo.mainWith "test" ["--help"]
