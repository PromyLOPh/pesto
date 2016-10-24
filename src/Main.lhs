User interface
++++++++++++++

.. class:: nodoc

> module Main (main) where
> import System.Environment (getArgs)
> import Data.List (intercalate)
> import Data.Monoid ((<>))
>
> import Codec.Pesto.Parse (parse, Instruction (Ingredient), Quantity (..))
> import Codec.Pesto.Graph (extract, toGraph, firstNodeId, resolveReferences)
> import Codec.Pesto.Lint (lint, extractMetadata, Metadata(..), LintResult (LintResult))
> import Codec.Pesto.Serialize (serialize)

The user-interface has different modes of operation. All of them read a single
recipe from the standard input.

> main = do
> 	(op:_) <- getArgs
> 	s <- getContents
>	either malformedRecipe (run op) (parse s)

> run "dot" = runDot
> run "metadata" = runMeta
> run "ingredients" = runIngredients
> run _ = const (putStrLn "unknown operation")

> malformedRecipe = print

> streamToGraph stream = (nodes, edges)
> 	where
> 		doc = (head . extract . snd . unzip) stream
> 		nodes = zip [firstNodeId..] doc
> 		edges = toGraph nodes ++ resolveReferences nodes

dot
^^^

Since each recipe is just a directed graph (digraph), GraphViz’ dot language
can represent recipes as well. Example:

.. code:: bash

	cabal run --verbose=0 pesto dot < spaghetti.pesto | dot -Tpng > spaghetti.png

> runDot stream = putStrLn $ toDot dotNodes dotEdges
> 	where
> 		(nodes, edges) = streamToGraph stream
> 		maxId = (maximum $ map fst nodes) + 1
> 		(lintNodes, lintEdges) = unzip $ map (uncurry lintToNodesEdges)
> 				$ zip [maxId..] (lint nodes edges)
> 		dotNodes = concat [
>				  [("node", [("fontname", "Roboto Semi-Light")])]
> 				, map (\(a, label) -> (show a, [("label", serialize label)])) nodes
> 				, lintNodes
>				]
> 		dotEdges = concat [
> 				  map (both show) edges
> 				, concat lintEdges
> 				]

> lintToNodesEdges nodeid (LintResult t nodes) = let
> 	n = (show nodeid, [("label", show t), ("color", "red")])
> 	e = map (\i -> both show (nodeid, i)) nodes
> 	in (n, e)

> both f (a, b) = (f a, f b)

> toDot nodes edges = "digraph a {"
> 		<> mconcat (map nodeToDot nodes)
> 		<> mconcat (map edgeToDot edges)
> 		<> "}"
> 	where
> 		edgeToDot (a, b) = a <> " -> " <> b <> ";"
>		nodeToDot (a, b) = a <> " [" <> mconcat (mapToDot b) <> "];"

> mapToDot = map kvToDot
> kvToDot (k, v) = k <> "=\"" <> quoteString v <> "\""
> quoteString s = mconcat $ map quoteChar s
> quoteChar '\n' = "\\n"
> quoteChar '"' = "\\\""
> quoteChar x = [x]

metadata
^^^^^^^^

Print metadata as key-value pairs, separated by ``=``.

> runMeta stream = maybe (return ()) (mapM_ printMeta) $ uncurry extractMetadata $ streamToGraph stream

ingredients
^^^^^^^^^^^

Extract ingredients and print them in CSV format. This does not take
alternatives into account yet.

> runIngredients stream = mapM_ (putStrLn . csvQty) $ reverse $ foldl getIngredient [] stream
> 	where
> 		getIngredient xs (_, Ingredient q) = q:xs
> 		getIngredient xs _ = xs

> printMeta (_, (key, MetaStr value)) = putStrLn $ key ++ "=" ++ value
> printMeta (_, (key, MetaQty q)) = putStrLn $ key ++ "=" ++ csvQty q

> csvQty (Quantity a b c) = intercalate "," [serialize a, b, c]

