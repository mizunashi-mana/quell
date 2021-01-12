module Main where

import           Prelude

import qualified Build_doctests     as BuildF
import           Control.Monad
import qualified System.Environment as IO
import qualified System.IO          as IO
-- import qualified Test.DocTest       as DocTest


main :: IO ()
main = forM_ BuildF.components \(BuildF.Component name flags pkgs sources) -> do
  putStrLn "============================================="
  print name
  putStrLn "---------------------------------------------"
  IO.hFlush IO.stdout
  let args = flags ++ pkgs ++ sources
  IO.unsetEnv "GHC_ENVIRONMENT"
  print args
  -- DocTest.doctest args
  putStrLn "============================================="
  IO.hFlush IO.stdout
