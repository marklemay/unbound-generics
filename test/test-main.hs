module Main where

import System.Environment
import Test.Tasty

import TestCalc
import TestParallelReduction
import PropOpenClose
import TinyLam
import TestACompare
import TestRefine
import TestIgnore
import TestShiftEmbed
import TestTH
import TestSubstBind

main :: IO ()
main = do
  setEnv "TASTY_TIMEOUT" "120s"

  setEnv "TASTY_QUICKCHECK_TESTS" "100000"
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "5000"

  defaultMain $ testGroup "unboundGenerics"
       [
         test_calc
       , test_parallelReduction
       , test_openClose
       , test_refine
       , test_ignore
       , test_tinyLam
       , test_acompare
       , test_shiftEmbed
       , test_TH
       , test_substBind
       ]
  unsetEnv "TASTY_TIMEOUT"
  unsetEnv "TASTY_QUICKCHECK_TESTS"
  unsetEnv "TASTY_QUICKCHECK_MAX_SIZE"