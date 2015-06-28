Language semantics
------------------

.. class:: nodoc

> module Codec.Pesto.Graph (
> 	  toGraph
> 	, walkRoot
> 	, outgoing
> 	, incoming
> 	, firstNodeId
> 	, resolveReferences
> 	, test
> 	, extract
> 	) where
> import Data.Char (isSpace, toLower, isLetter)
> import Data.List (sort, nub)
> import Test.HUnit hiding (test)
> import Control.Applicative ((<$>))
>
> import Codec.Pesto.Parse hiding (test)

The parser’s output, a stream of operations, may contain multiple recipes. A
recipe must start with the directive “pesto” and may end with “bonappetit”.
This function extracts all recipes from the stream and removes both directives.

- easily embed recipes into other documents

> extract [] = []
> extract (Directive "pesto":stream) = between:extract next
> 	where
> 		isEnd (Directive x) | x `elem` ["bonappetit", "pesto"] = True
> 		isEnd _ = False
> 		(between, next) = break isEnd stream
> extract (x:xs) = extract xs

Start and end directive are removed from the extracted operations.  The
directive “bonappetit” is optional at the end of a stream.

> testExtract = [
> 	  extract [Directive "pesto", Directive "bonappetit"] ~?= [[]]
> 	, extract [Directive "pesto", Action "foobar", Directive "bonappetit"] ~?= [[Action "foobar"]]
> 	, extract [Directive "pesto"] ~?= [[]]
> 	, extract [Directive "pesto", Directive "foobar"] ~?= [[Directive "foobar"]]

Operations surrounding the start and end directive are removed.

> 	, extract [Unknown "Something", Directive "pesto"] ~?= [[]]
> 	, extract [Unknown "Something", Action "pour", Directive "pesto"] ~?= [[]]
> 	, extract [Directive "pesto", Directive "bonappetit", Annotation "something"] ~?= [[]]

The stream may contain multiple recipes. The start directive also ends the
previous recipe and starts a new one.

> 	, extract [Directive "pesto", Action "pour", Directive "bonappetit", Action "foobar", Directive "pesto", Annotation "something"] ~?= [[Action "pour"], [Annotation "something"]]
> 	, extract [Directive "pesto", Action "heat", Directive "pesto", Annotation "something"] ~?= [[Action "heat"], [Annotation "something"]]
> 	, extract [Directive "pesto", Annotation "foobar", Directive "pesto", Directive "bonappetit"] ~?= [[Annotation "foobar"], []]
> 	]

Each recipe’s stream of operations drives a stack-based machine that transforms
it into a directed graph. Think of the stack as your kitchen’s workspace that
is used to prepare the food’s components. You can add new ingredients, perform
actions on them, put them aside and add them again.

This function processes a list of nodes, that is operations uniquely identified
by an integer and returns the edges of the directed graph as a list of tuples.

> toGraph nodes = edges
> 	where
> 		(_, _, edges) = foldl f (Nothing, [[]], []) nodes

Ingredients are simply added to the current workspace. They should for example
appear on the shopping list.

> 		f ctx (i, Ingredient _) = addToStack ctx i

The same happens for for tools. However they are not part of the final product,
but used in the process of making it.  For instance they do not appear on the
shopping list. `Time is a tool <time-is-a-tool_>`_.

> 		f ctx (i, Tool _) = addToStack ctx i

Actions take all ingredients and tools currently on the workspace, perform some
action with them and put the product back onto the workspace.

> 		f (_, stack:sx, edges) (i, Action _) = (Just i, [i]:stack:sx, edgesTo i stack ++ edges)

Results add a label to the current workspace’s contents and move them out of
the way. It should be a meaningful name, not just A and B obviously.
Consecutive Results add different labels to the same workspace. That’s useful
when an action yields multiple results at once that are processed in different
ways.

> 		f ctx (i, Result _) = consumeStack ctx i

Alternatives too add a label to the current workspace’s content, but they pick
one of things on the workspace and throw everything else away. This allows
adding optional or equivalent ingredients to a recipe (i.e. margarine or butter).

> 		f ctx (i, Alternative _) = consumeStack ctx i

References are similar to ingredients. They are used to add items from a
workspace labeled with Result or Alternative. More on that `in the next section
<references_>`_.

> 		f ctx (i, Reference _) = addToStack ctx i

Annotations add a description to any of the previous operations. They can be
used to provide more information about ingredients (so “hot water” becomes
“+water (hot)”, tools (“&oven (200 °C)”) or actions (“[cook] (XXX)”).

> 		f ctx@(Nothing, s, edges) (_, Annotation _) = ctx
> 		f (Just prev, s, edges) (i, Annotation _) = (Just prev, s, (i, prev):edges)

Unused directives or unknown operations are danging nodes with no connection to
other nodes.

> 		f ctx (_, Directive _) = ctx
> 		f ctx (_, Unknown _) = ctx

These are helper functions:

> 		addToStack (_, stack:sx, edges) i = (Just i, (i:stack):sx, edges)
> 		consumeStack (_, s, edges) i =
> 			let
> 				stack = dropWhile null s
> 				(top:sx) = if null stack then [[]] else stack
> 			in (Just i, []:top:sx, edgesTo i top ++ edges)
> 		edgesTo i = map (\x -> (x, i))

Here are a few example of how this stack-machine works. Each edge is a tuple of
two integer numbers. These are the nodes it connects, starting with zero.
Ingredient, Tool and Reference itself do not create any edges:

> testGraph = [
> 	  cmpGraph "+ketchup &spoon *foobar" []

But Action, Alternative and Result do in combination with them:

> 	, cmpGraph "+foobar [barbaz]" [(0, 1)]
> 	, cmpGraph "+foobar |barbaz" [(0, 1)]
> 	, cmpGraph "+foobar >barbaz" [(0, 1)]
> 	, cmpGraph "+foobar +B >barbaz" [(0, 2), (1, 2)]
> 	, cmpGraph "+foobar >barbaz +foobar >barbaz" [(0, 1), (2, 3)]
> 	, cmpGraph "+foobar [barbaz] +foobar >barbaz" [(0, 1), (1, 3), (2, 3)]
> 	, cmpGraph "&foobar [barbaz] [C] >D" [(0, 1), (1, 2), (2, 3)]

If the stack is empty, i.e. it was cleared by a Result or Alternative
operation, consecutive results or alternatives operate on the *previous*,
non-empty stack.

> 	, cmpGraph "+foobar >barbaz >C" [(0, 1), (0, 2)]
> 	, cmpGraph "+foobar |barbaz |C" [(0, 1), (0, 2)]
> 	, cmpGraph "+foobar >barbaz |C" [(0, 1), (0, 2)]

Unless that stack too is empty. Then they do nothing:

> 	, cmpGraph ">foobar >foobar" []
> 	, cmpGraph "|foobar |foobar" []
> 	, cmpGraph "(foobar) (foobar)" []
> 	, cmpGraph "[foobar]" []

The Annotation operation always creates an edge to the most-recently processed
node that was not an annotation. Thus two consecutive annotations create edges
to the same node.

> 	, cmpGraph "+foobar (barbaz)" [(1, 0)]
> 	, cmpGraph "+foobar (barbaz) (C)" [(1, 0), (2, 0)]
> 	, cmpGraph "+foobar (barbaz) >barbaz" [(1, 0), (0, 2)]
> 	, cmpGraph "+foobar >barbaz (C)" [(0, 1), (2, 1)]
> 	, cmpGraph "+foobar |barbaz (C)" [(0, 1), (2, 1)]
> 	, cmpGraph "*foobar (C)" [(1, 0)]

Unknown directives or operations are never connected to other nodes.

> 	, cmpGraph "%invalid" []
> 	, cmpGraph "invalid" []
> 	]

References
++++++++++

Results and alternatives can be referenced with the Reference operation.
Resolving these references does not happen while buiding the graph, but
afterwards. This allows referencing an a result or alternative before its
definition with regard to the their processing order.

Resolving references is fairly simple: For every reference its object name a
case-insensitive looked is performed in a table containing all results and
alternatives. If it succeeds an edge from every result and alternative returned
to the reference in question is created.

> resolveReferences nodes = foldl f [] nodes
> 	where
>		f edges (i, ref@(Reference _)) = map (\x -> (x, i)) (findTarget nodes ref) ++ edges
> 		f edges _ = edges

> findTarget nodes (Reference (Quantity _ _ a)) = map fst $ filter (isTarget a) nodes
> 	where
> 		lc = map toLower
> 		isTarget dest (_, Result x) = lc x == lc dest
>		isTarget dest (_, Alternative x) = lc x == lc dest
> 		isTarget _ _ = False


References works before or after the result operation.

> testRef = [
> 	  cmpGraphRef ">foobar *foobar" [(0, 1)]
> 	, cmpGraphRef ">foobar |foobar *foobar" [(0, 2), (1, 2)]
> 	, cmpGraphRef "+A >foobar +B >barbaz *foobar *barbaz" [(1, 4), (3, 5)]
> 	, cmpGraphRef "*foobar >foobar" [(1, 0)]

Nonexistent references do not create an edge.

> 	, cmpGraphRef ">foobar *barbaz" []

References can use amounts and units.

> 	, cmpGraphRef ">foobar *1 _ foobar *2 _ foobar" [(0, 1), (0, 2)]

There are a few cases that do not make sense here (like loops or multiple
results with the same name). They are permitted at this stage, but rejected
`later <reject-loops_>`_.

> 	, cmpGraphRef "*foobar |foobar >foobar" [(1, 0), (2, 0)]
> 	, cmpGraphRef "|foobar *foobar >foobar *foobar" [(0, 1), (0, 3), (2, 1), (2, 3)]
> 	]

Appendix
++++++++

> runGraphWith f doc expect = sort edges ~?= sort expect
> 	where
> 		(Right op) = (head . extract . snd . unzip) <$> parse ("%pesto " ++ doc)
> 		nodes = zip [firstNodeId..] op
> 		edges = f nodes
> cmpGraph = runGraphWith toGraph
> cmpGraphRef = runGraphWith resolveReferences

> firstNodeId = 0 :: Int

Find graph’s root node(s), that is a node without outgoing edges:

> walkRoot nodes edges = let out = nub $ map fst edges
>	in filter (\(x, _) -> notElem x out) nodes

Get all nodes with edges pointing towards nodeid

> incoming edges (nodeid, _) = filter ((==) nodeid . snd) edges

> outgoing edges (nodeid, _) = filter ((==) nodeid . fst) edges

> test = ["graph" ~: testGraph, "ref" ~: testRef, "extract" ~: testExtract]

