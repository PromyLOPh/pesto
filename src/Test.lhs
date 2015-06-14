Running tests
+++++++++++++

.. class:: nodoc

> import Test.HUnit
> import Codec.Pesto.Parse (test)
> import Codec.Pesto.Lint (test)
> import Codec.Pesto.Graph (test)

The testcases can be run with ``cabal run pesto-test``. This runs *all*
testcases from all modules and prints a summary.

> main = runTestTT tests

> tests = TestList [
> 	  "parse" ~: Codec.Pesto.Parse.test
> 	, "graph" ~: Codec.Pesto.Graph.test
> 	, "lint" ~: Codec.Pesto.Lint.test
> 	]

