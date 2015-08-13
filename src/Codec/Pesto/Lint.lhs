Linting
-------

.. class:: nodoc

> module Codec.Pesto.Lint (lint
> 	, test
> 	, parseMetadata
> 	, extractMetadata
> 	, Metadata(..)
> 	, LintResult(..)) where
> import Test.HUnit hiding (test, Node)
> import Data.List (sort, isPrefixOf)
> import Control.Applicative ((<*>), (<$>), (*>))
> import Text.Parsec hiding (parse)
> import Data.Char (isSpace)
> import Data.Ratio ((%))
> import Data.Maybe (fromMaybe)
>
> import Codec.Pesto.Graph hiding (test)
> import Codec.Pesto.Parse hiding (test)

Not every graph generated in the previous section is a useful recipe. Some
instruction sequences just do not make sense. The tests in this section can
detect those. Failing any of them does not render a stream of instructions or
graph invalid. They just does not describe a *useful* recipe. Thus
implementations must not generate or export such documents. However they should
accept input that fails any of the tests and warn the user about the failure.

Additionally this section provides guidance on how to use the instructions
provided by the Pesto language properly.

Graph properties
++++++++++++++++

- weakly connected, no dangling nodes/subgraphs
- acyclic

The graph must have exactly one root node (i.e. a node with incoming edges
only).  This also requires all results and alternatives to be referenced
somewhere. Directives are either consumed when parsing, generating a graph and
linting. Otherwise they are dangling as well. Unknown instructions are always
dangling.

> rootIsResult nodes edges = case walkRoot nodes edges of
> 	[] -> [LintResult NoRootNode []]
> 	(_, Result _):[] -> []
> 	(i, _):[] -> [LintResult NonResultRootNode [i]]
> 	xs -> [LintResult MoreThanOneRootNode (map fst xs)]

Empty recipes or circular references have no root node:

> testConnectivity = [
> 	  cmpLint "" [LintResult NoRootNode [], LintResult NoMetadata []]
> 	, cmpLint "*foobar >foobar"
>		[LintResult NoRootNode [], LintResult NoMetadata []]
> 	, cmpLint "+foobar"
>		[LintResult NonResultRootNode [0], LintResult NoMetadata []]

Directives and unknown instructions are dangling and thus root nodes.

> 	, cmpLint "invalid %invalid +foo >bar"
> 		[LintResult MoreThanOneRootNode [0,1,3], LintResult NoMetadata []]
>	]

Metadata
++++++++

.. _resultsused:

.. class:: todo

root node can be alternative too?

The graph’s root node must be a result. It contains yield (amount and unit) and
title (object) of the recipe.

> extractMetadata nodes edges = case walkRoot nodes edges of
> 		[n@(i, Result q@(Quantity _ _ title))] ->
> 			Just $ (i, ("title", MetaStr title))
> 				:(i, ("yield", MetaQty q))
> 				:foldl f [] (incomingNodes nodes edges n)
>		_ -> Nothing
> 	where

Additional key-value metadata for the whole recipe can be added as annotations
to the root node. If multiple annotations with the same key exist the key maps
to a list of those values. Annotations that are unparseable key-value pairs are
added as recipe description instead.

> 		f xs (i, Annotation s) = case parseMetadata s of
> 				Left _ -> (i, ("description", MetaStr s)):xs
> 				Right (k, v) -> (i, (k, MetaStr v)):xs
> 		f xs _ = xs

Key and value are separated by a colon. Keys must not contain whitespace or the
colon char. A value may be empty.

> parseMetadata = runParser metadata () ""
> metadata = let keychars = satisfy (\x -> not (isSpace x) && x /= ':') in (,)
> 	<$> many1 keychars
> 	<*> (char ':' *> spaces *> many anyChar)

> lintMetadata nodes edges = case extractMetadata nodes edges of
> 		Just result -> foldl checkKey [] result
> 		Nothing -> [LintResult NoMetadata []]
> 	where
> 		checkKey xs (_, (k, _)) | isKeyKnown k = xs
> 		checkKey xs (i, _) = LintResult UnknownMetadataKey [i]:xs

Valid metadata keys are listed below. Additionally applications may add keys by
prefixing them with “x-myapp-”, thus an application called “basil” adding
“some-key” would use the full key “x-basil-some-key”.

> 		isKeyKnown k = k `elem` knownKeys || "x-" `isPrefixOf` k

The following metadata keys are permitted:

> 		knownKeys = [

Both, title and description, are implicit.

>			  "title"
> 			, "description"

The recipe’s language, as 2 character code (`ISO 639-1`_).

.. _ISO 639-1: http://www.loc.gov/standards/iso639-2/php/English_list.php

> 			, "language"

Yield and time both must be a quantity.

> 			, "yield"
> 			, "time"

An image can be a relative file reference or URI

> 			, "image"
> 			, "author"
> 			]

.. class:: todo

Check the metadata’s value format. I.e. yield/time must be quantity

For instance a german language recipe for one person would look like this:

> testMetadata = [
> 	  cmpLintMeta "+foo >1 ml foobar (language: de) (x-app-key: value)"
>		[]
>		(Just [(1, ("title", MetaStr "foobar"))
> 			, (1, ("yield", MetaQty (Quantity (Exact (AmountRatio (1%1))) "ml" "foobar")))
>			, (2, ("language", MetaStr "de"))
> 			, (3, ("x-app-key", MetaStr "value"))])

Unparseable annotations or unknown keys are linting errors:

> 	, cmpLintMeta "+foo >foobar (unknown-key: value)"
> 		[LintResult UnknownMetadataKey [2]]
> 		(Just [(1, ("title", MetaStr "foobar"))
>			, (1, ("yield", MetaQty (strQuantity "foobar")))
> 			, (2, ("unknown-key", MetaStr "value"))])

Root node annotations not containing a parseable key-value pair are assigned
the key “description”.

> 	, cmpLintMeta "+foo >foobar ( some description ) (another one: with colon) (another:     valid key-value)"
> 		[LintResult UnknownMetadataKey [4]]
> 		(Just [(1, ("title", MetaStr "foobar"))
> 			, (1, ("yield", MetaQty (strQuantity "foobar")))
> 			, (2, ("description", MetaStr " some description "))
> 			, (3, ("description", MetaStr "another one: with colon"))
> 			, (4, ("another", MetaStr "valid key-value"))])
> 	]

.. _time-is-a-tool:

Time is a tool
++++++++++++++

By definition time is a tool and not an ingredient.

> timeUnits = ["s", "min", "h", "d"]
>
> isTime (Quantity _ unit "") | unit `elem` timeUnits = True
> isTime _ = False

> timeIsATool nodes _ = foldl f [] nodes
> 	where
> 		f xs (nodeid, Ingredient q) | isTime q = LintResult TimeIsATool [nodeid]:xs
>		f xs _ = xs

> testLintQuantity = [
> 	  cmpLint "+10 min >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "+10-12 h >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "+90/120 s >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "+~12 s >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "&10 min [bar] >foo" []
> 	]

Only actions can be annotated with a time. It can be used to indicate how long
a certain action is *expected* to take (i.e. peeling potatoes takes two
minutes) or how long the action is supposed to be executed (i.e. cook five
minutes). More time annotations improve the software’s scheduling capabilities.

> timeAnnotatesAction nodes edges = foldl f [] nodes
> 	where
> 		f xs n@(nodeid, Tool q) | isTime q && (not . allActions) (outgoingEdges edges n) = LintResult TimeAnnotatesAction [nodeid]:xs
> 		f xs _ = xs
> 		toNodelist = (!!) nodes . snd
> 		allActions = all (isAction . snd . toNodelist)

For example “cook 10 minutes” can be expressed with

> testTimeAnnotatesAction = [
> 	  cmpLint "&10 min [cook] >soup" []
> 	, cmpLint "&10 min [cook] &5-6 h [cook again] >soup" []
> 	, cmpLint "&10 min >soup" [LintResult TimeAnnotatesAction [0]]
> 	, cmpLint "&10 min &15 min |time *time [cook] >soup"
> 		[LintResult TimeAnnotatesAction [0], LintResult TimeAnnotatesAction [1]]
> 	]

.. _well-known-units:

Well-known units
++++++++++++++++

Units can be an arbitrary strings, but implementations should recognize the
common metric units g (gram), l (litre) and m (metre). One of these prefixes
may be used with each of them: m (milli-), c (centi-), d (dezi-) and k (kilo-).
Additionally time in s (second), min (minute), h (hour), d (day) should be
accepted.

> wellKnownUnit nodes _ = foldl f [] nodes
> 	where
> 		extractQty (Ingredient q) = Just q
> 		extractQty (Tool q) = Just q
> 		extractQty (Result q) = Just q
> 		extractQty (Alternative q) = Just q
> 		extractQty (Reference q) = Just q
> 		extractQty _ = Nothing
> 		f xs (nodeid, instr) | fromMaybe False (extractQty instr >>= (return . not . known)) =
> 				LintResult UnitNotWellKnown [nodeid]:xs
>		f xs _ = xs
> 		known (Quantity _ unit _) = unit `elem` knownUnits
> 		knownUnits = [
> 				  ""
> 				, "mg", "g", "kg"
> 				, "ml", "cl", "dl", "l"
> 				, "cm", "dm", "m"
> 				] ++ timeUnits

Usage of imperial units (inch, pound, …) as well as non-XXX units like
“teaspoon”, “cup”, … is discouraged because the former is used by just three
countries in the world right now and the latter is language- and
country-dependent. The implementation may provide the user with a conversion
utility.

.. class:: todo

- example: 1 oz ~= 28.349523125 g, can only be approximated by rational number, for instance 29767/1050 g
- 15 oz would are :math:`\frac{29767}{70} \mathrm{g} = 425+\frac{17}{70} \mathrm{g}`, since nobody sells 17/70 g the implementation would round down to ~425 g (although <1g is not really enough to justify adding approx)

> testLintWellKnownUnit = [
> 	  cmpLint "+1 in foobar >bar" [LintResult UnitNotWellKnown [0]]
> 	, cmpLint "+2 teaspoons foobar >bar" [LintResult UnitNotWellKnown [0]]
> 	, cmpLint "+3 cups foobar >bar" [LintResult UnitNotWellKnown [0]]
> 	, cmpLint "+1 ml foobar >bar" []
> 	, cmpLint "+1 cl foobar >bar" []
> 	, cmpLint "+1 dl foobar >bar" []
> 	, cmpLint "+1 l foobar >bar" []
> 	, cmpLint "+2 _ something >bar" []
> 	, cmpLint "&1 min [foo] >bar" []

The unit is case-sensitive, thus

.. class:: todo

Should we allow case-insensitive units? References are case-insensitive as
well…

> 	, cmpLint "+1 Mg foobar >bar" [LintResult UnitNotWellKnown [0]]
> 	, cmpLint "+1 kG foobar >bar" [LintResult UnitNotWellKnown [0]]
> 	, cmpLint "&1 MIN [foo] >bar" [LintResult UnitNotWellKnown [0]]
> 	]

References
++++++++++

All references must be resolved. An `earlier check <resultsused_>`_ makes sure
all results and alternatives are referenced at some point.

> referencesResolved nodes edges = foldl f [] nodes
> 	where
> 		f xs n@(nodeid, Reference _) | null (incomingEdges edges n) =
> 				LintResult UndefinedReference [nodeid]:xs
>		f xs _ = xs

> testLintRefs = [
> 	  cmpLint "*foobar >foobar >barbaz" []
> 	, cmpLint "*foobar >foo" [LintResult UndefinedReference [0]]
> 	]

A result must have at least one incoming edge. This is a special case and can
only occur at the beginning of a recipe.

> resultNonempty nodes edges = foldl f [] nodes
> 	where
> 		f xs n@(nodeid, Result _) | null (incomingEdges edges n) =
> 				LintResult TooFewChildren [nodeid]:xs
>		f xs _ = xs

> testLintResultNonempty = [
> 	  cmpLint ">bar *bar >baz" [LintResult TooFewChildren [0]]
> 	, cmpLint "+A >bar *bar >baz" []
> 	, cmpLint "+A >bar >foo *bar *foo >baz" []
> 	]

Alternatives must have at least two incoming edges since a smaller amount would
make the alternative pointless.

> twoAlternatives nodes edges = foldl f [] nodes
> 	where
>		f xs n@(nodeid, Alternative _) | length (incomingEdges edges n) < 2 =
> 				LintResult TooFewChildren [nodeid]:xs
>		f xs _ = xs

> testLintTwoAlternatives = [
> 	  cmpLint "+A |foo *foo >bar" [LintResult TooFewChildren [1]]
> 	, cmpLint "+A +B |foo *foo >bar" []

.. class:: todo

should we allow this? it does not make sense imo

> 	, cmpLint "+A &B |foo *foo >bar" []
> 	]

.. _reject-loops:

.. class:: todo

- reject loops
- reject multiple results/alternatives with the same name

Ranges
++++++

The first amount of a range ratio must be strictly smaller than the second.
This limitation is not enforced for ranges containing strings.

> rangeFromLargerThanTo nodes _ = foldl f [] nodes
> 	where
>		f xs (nodeid, Ingredient q) | not $ rangeOk q =
> 				LintResult RangeFromLargerThanTo [nodeid]:xs
>		f xs (nodeid, Reference q) | not $ rangeOk q =
> 				LintResult RangeFromLargerThanTo [nodeid]:xs
>		f xs _ = xs
> 		rangeOk (Quantity (Range (AmountRatio a) (AmountRatio b)) _ _) = a < b
> 		rangeOk _ = True

> testRangeFromLargerThanTo = [
> 	  cmpLint "+2-3 l water >bar" []
> 	, cmpLint "+3-2 l water >bar" [LintResult RangeFromLargerThanTo [0]]
> 	, cmpLint "+2/3-1/3 l water >bar" [LintResult RangeFromLargerThanTo [0]]
> 	, cmpLint "+some-many _ eggs >bar" []
> 	, cmpLint "+1-\"a few\" _ eggs >bar" []
> 	]

Appendix
++++++++

> data LintResult = LintResult LintStatus [NodeId] deriving (Show, Eq, Ord)
> data LintStatus =
> 	  NoRootNode
> 	| NonResultRootNode
> 	| MoreThanOneRootNode
> 	| UndefinedReference
> 	| TooFewChildren
>	| TimeIsATool
> 	| TimeAnnotatesAction
>	| UnitNotWellKnown
> 	| InvalidNode
> 	| RangeFromLargerThanTo
> 	| NoMetadata
> 	| UnknownMetadataKey
> 	deriving (Show, Eq, Ord)

Every lint test checks a single aspect of the graph.

> lint nodes edges = concatMap (\f -> f nodes edges) lintTests

> lintTests = [
> 	  rootIsResult
>	, referencesResolved
>	, resultNonempty
>	, twoAlternatives
> 	, timeIsATool
> 	, timeAnnotatesAction
> 	, wellKnownUnit
> 	, lintMetadata
> 	, rangeFromLargerThanTo
> 	]

> withGraph doc f = f nodes edges
>	where
> 		(Right op) = (head . extract . snd . unzip) <$> parse ("%pesto " ++ doc)
> 		nodes = zip [firstNodeId..] op
> 		edges = toGraph nodes ++ resolveReferences nodes

> cmpLint doc expect = withGraph doc (\nodes edges -> doc ~: sort (lint nodes edges) ~?= sort expect)


> data Metadata = MetaQty Quantity | MetaStr String deriving (Show, Eq)

> cmpLintMeta doc expectLint expectMeta = withGraph doc (\nodes edges -> doc ~: [
> 		  sort (lint nodes edges) ~?= sort expectLint
> 		, extractMetadata nodes edges ~?= expectMeta
> 		])
> strQuantity = Quantity (Exact (AmountStr "")) ""

> test = [
> 	  testConnectivity
> 	, testMetadata
> 	, testLintRefs
> 	, testLintQuantity
> 	, testLintWellKnownUnit
> 	, testTimeAnnotatesAction
> 	, testLintTwoAlternatives
> 	, testLintResultNonempty
> 	, testRangeFromLargerThanTo
> 	]

