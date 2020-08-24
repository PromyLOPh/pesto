Running tests
+++++++++++++

.. class:: nodoc

> import Test.HUnit
> import System.Exit (exitFailure, exitSuccess)
> import Codec.Pesto.Parse (test)
> import Codec.Pesto.Lint (test)
> import Codec.Pesto.Graph (test)

The testcases can be run with ``cabal test``. This runs *all* testcases from
all modules and prints a summary.

> main = runTestTT tests >>= \c -> if errors c + failures c > 0 then exitFailure else exitSuccess

> tests = TestList [
> 	  "parse" ~: Codec.Pesto.Parse.test
> 	, "graph" ~: Codec.Pesto.Graph.test
> 	, "lint" ~: Codec.Pesto.Lint.test
> 	]

