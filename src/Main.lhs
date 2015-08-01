User interface
++++++++++++++

.. class:: nodoc

> module Main (main) where
> import System.IO (hPrint, stderr)
> import Codec.Pesto.Parse (parse)
> import Codec.Pesto.Graph (extract, toGraph, firstNodeId, resolveReferences)
> import Codec.Pesto.Lint (lint, extractMetadata)
> import Codec.Pesto.Dot (toDot)

The pesto to dot converter can be run with ``cabal run pesto``. It expects a
pesto recipe on the standard input and prints a dot graph to stdout that can be
converted to an image by piping it through ``dot -Tpng``. Example:

.. code:: bash

	cabal run --verbose=0 pesto < spaghetti.pesto | dot -Tpng > spaghetti.png

.. class:: todo

add linting information to graph

> main = do
> 	s <- getContents
>	(flip . either) malformedRecipe (parse s) $ \stream -> do
> 		let
> 			doc = (head . extract . snd . unzip) stream
> 			nodes = zip [firstNodeId..] doc
> 			edges = toGraph nodes ++ resolveReferences nodes
> 		hPrint stderr $ extractMetadata nodes edges
> 		hPrint stderr $ lint nodes edges
> 		putStrLn $ toDot nodes edges

> malformedRecipe = print

