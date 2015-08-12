User interface
++++++++++++++

.. class:: nodoc

> module Main (main) where
> import System.IO (hPrint, stderr)
> import System.Environment (getArgs)
> import Data.List (intercalate)
>
> import Codec.Pesto.Parse (parse, Instruction (Ingredient), Quantity (..))
> import Codec.Pesto.Graph (extract, toGraph, firstNodeId, resolveReferences)
> import Codec.Pesto.Lint (lint, extractMetadata, Metadata(..))
> import Codec.Pesto.Dot (toDot)
> import Codec.Pesto.Serialize (serialize)

The user-interface has different modes of operation. All of read a single
recipe from the standard input.

> main = do
> 	(op:_) <- getArgs
> 	s <- getContents
>	either malformedRecipe (run op) (parse s)

> malformedRecipe = print

> streamToGraph stream = (nodes, edges)
> 	where
> 		doc = (head . extract . snd . unzip) stream
> 		nodes = zip [firstNodeId..] doc
> 		edges = toGraph nodes ++ resolveReferences nodes

dot
^^^

Convert recipe into GraphViz’ dot language. Example:

.. code:: bash

	cabal run --verbose=0 pesto dot < spaghetti.pesto | dot -Tpng > spaghetti.png

.. class:: todo

add linting information to graph

> run "dot" stream = do
> 		let (nodes, edges) = streamToGraph stream
> 		hPrint stderr $ lint nodes edges
> 		putStrLn $ toDot nodes edges

metadata
^^^^^^^^

Print metadata as key-value pairs, separated by ``=``.

> run "metadata" stream = maybe (return ()) (mapM_ printMeta) $ uncurry extractMetadata $ streamToGraph stream

ingredients
^^^^^^^^^^^

Extract ingredients and print them in CSV format. This does not take
alternatives into account yet.

> run "ingredients" stream = mapM_ (putStrLn . csvQty) $ reverse $ foldl getIngredient [] stream
> 	where
> 		getIngredient xs (_, Ingredient q) = q:xs
> 		getIngredient xs _ = xs
> run _ _ = putStrLn "unknown operation"

> printMeta (_, (key, MetaStr value)) = putStrLn $ key ++ "=" ++ value
> printMeta (_, (key, MetaQty q)) = putStrLn $ key ++ "=" ++ csvQty q

> csvQty (Quantity a b c) = intercalate "," [serialize a, b, c]

