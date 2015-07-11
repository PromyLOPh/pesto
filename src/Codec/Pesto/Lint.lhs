Linting
-------

.. class:: nodoc

> module Codec.Pesto.Lint (lint, test, parseMetadata) where
> import Test.HUnit hiding (test)
> import Data.List (sort, isPrefixOf)
> import Control.Applicative ((<*>), (<$>), (<*), (*>))
> import Text.Parsec hiding (parse)
> import Text.Parsec.Char
>
> import Codec.Pesto.Graph hiding (test)
> import Codec.Pesto.Parse hiding (test)

Not every graph generated in the previous section is a useful recipe, since
some combinations of instructions just do not make sense. The linting test in
this section can detect common errors. Failing any of these tests does not
render a recipe invalid, but *useless*. Thus implementations must not create
such recipes. They may be accepted the user though.

Every lint test checks a single aspect of the graph.

> lint nodes edges = concatMap (\f -> f nodes edges) lintTests

Metadata
++++++++

.. _recipetitle:
.. _resultsused:

The graph must have exactly one root node (i.e. a node with incoming edges
only) and it must be a result. The result’s object name is the recipe’s title.
This also requires all results and alternatives to be referenced somewhere.
Directives are either consumed when parsing, generating a graph and linting.
Otherwise they are dangling as well. Unknown instructions are always dangling.

> rootIsResult nodes edges = case walkRoot nodes edges of
> 	[] -> [LintResult NoRootNode []]
> 	(i, x):[] -> if isResult x then [] else [LintResult NonResultRootNode [i]]
> 	xs -> [LintResult MoreThanOneRootNode (map fst xs)]

Empty recipes or circular references have no root node:

> testLintMetadata = [
> 	  cmpLint "" [LintResult NoRootNode []]
> 	, cmpLint "*foobar >foobar" [LintResult NoRootNode []]
> 	, cmpLint "+foobar" [LintResult NonResultRootNode [0]]

This recipe’s title is “Pesto”.

> 	, cmpLint "+foobar >Pesto" []

Directives and unknown instructions are dangling and thus root nodes.

> 	, cmpLint "invalid %invalid +foo >bar" [LintResult MoreThanOneRootNode [0,1,3]]
>	]

Additional key-value metadata for the whole recipe can be provided by adding
annotations to the the root node. If multiple annotations with the same key
exist the key maps to a list of those values.

> parseMetadata = runParser metadata () ""
> metadata = (,)
> 	<$> (char '.' *> many1 (noneOf ":"))
> 	<*> (char ':' *> spaces1 *> many1 anyChar)

> rootAnnotations nodes edges = foldl check [] rootIncoming
> 	where
> 		rootIncoming = map ((!!) nodes . fst) $ concatMap (incoming edges) $ walkRoot nodes edges
> 		check xs (i, Annotation s) | "." `isPrefixOf` s = case parseMetadata s of
> 			(Left _) -> LintResult InvalidMetadata [i]:xs
> 			(Right (k, v)) -> if isKeyKnown k
> 				then xs
> 				else LintResult UnknownMetadataKey [i]:xs
> 		check xs _ = xs

.. class:: todo

reject metadata annotations for non-root nodes

The valid keys are listed below. Additionally applications may add their own
metadata with “x-appname-keyname”.

> 		isKeyKnown k = k `elem` knownKeys || "x-" `isPrefixOf` k

The following metadata keys are permitted:

> 		knownKeys = [

The recipe’s language, as 2 character code (ISO 639-1:2002).

> 			  "language"

Yield and time both must be a quantity.

> 			, "yield"
> 			, "time"

An image can be a relative file reference or URI

> 			, "image"
> 			, "author"
> 			]

For instance a german language recipe for one person would look like this:

> testRootAnnotations = [
> 	  cmpLint "+foo >foobar (.language: de) (.yield: 1 _ Person) (.x-app-this: value)" []

Unparseable annotations or unknown keys are linting errors:

> 	, cmpLint "+foo >foobar (.invalid)" [LintResult InvalidMetadata [2]]
> 	, cmpLint "+foo >foobar (.unknown: value)" [LintResult UnknownMetadataKey [2]]

Root node annotations not starting with a dot are considered recipe descriptions.

> 	, cmpLint "+foo >foobar (some description)" []
> 	]

.. _time-is-a-tool:

Time is a tool
++++++++++++++

By definition time is a tool and not an ingredient.

> timeUnits = ["s", "min", "h", "d"]
>
> isTime (Quantity _ unit "") | unit `elem` timeUnits = True
> isTime _ = False

> timeIsATool nodes edges = foldl f [] nodes
> 	where
> 		f xs n@(nodeid, Ingredient q) | isTime q = LintResult TimeIsATool [nodeid]:xs
>		f xs _ = xs

> testLintQuantity = [
> 	  cmpLint "+10 min >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "+10-12 h >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "+90/120 s >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "+~12 s >foo" [LintResult TimeIsATool [0]]
> 	, cmpLint "&10 min [bar] >foo" []
> 	]

Only actions can be annotated with a time.

> timeAnnotatesAction nodes edges = foldl f [] nodes
> 	where
> 		f xs n@(nodeid, Tool q) | isTime q && (not . allActions) (outgoing edges n) = LintResult TimeAnnotatesAction [nodeid]:xs
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

> wellKnownUnit nodes edges = foldl f [] nodes
> 	where
> 		f xs n@(nodeid, Ingredient q) | (not . known) q =
> 				LintResult UnitNotWellKnown [nodeid]:xs
> 		f xs n@(nodeid, Tool q) | (not . known) q =
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
> 		f xs n@(nodeid, Reference _) | null (incoming edges n) =
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
> 		f xs n@(nodeid, Result _) | null (incoming edges n) =
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
>		f xs n@(nodeid, Alternative _) | length (incoming edges n) < 2 =
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

> rangeFromLargerThanTo nodes edges = foldl f [] nodes
> 	where
>		f xs n@(nodeid, Ingredient q) | not $ rangeOk q =
> 				LintResult RangeFromLargerThanTo [nodeid]:xs
>		f xs n@(nodeid, Reference q) | not $ rangeOk q =
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

> data LintResult a = LintResult LintStatus [a] deriving (Show, Eq, Ord)
> data LintStatus =
> 	  NoRootNode
> 	| NonResultRootNode
> 	| MoreThanOneRootNode
> 	| UndefinedReference
> 	| TooFewChildren
>	| TimeIsATool
> 	| TimeAnnotatesAction
>	| UnitNotWellKnown
> 	| UnknownMetadataKey
> 	| InvalidMetadata
> 	| InvalidNode
> 	| RangeFromLargerThanTo
> 	deriving (Show, Eq, Ord)

> lintTests = [
> 	  rootIsResult
>	, referencesResolved
>	, resultNonempty
>	, twoAlternatives
> 	, timeIsATool
> 	, timeAnnotatesAction
> 	, wellKnownUnit
> 	, rootAnnotations
> 	, rangeFromLargerThanTo
> 	]

> cmpLint doc expect = doc ~: sort (lint nodes edges) ~?= sort expect
> 	where
> 		(Right op) = (head . extract . snd . unzip) <$> parse ("%pesto " ++ doc)
> 		nodes = zip [firstNodeId..] op
> 		edges = toGraph nodes ++ resolveReferences nodes

> test = [
> 	  testLintMetadata
> 	, testRootAnnotations
> 	, testLintRefs
> 	, testLintQuantity
> 	, testLintWellKnownUnit
> 	, testTimeAnnotatesAction
> 	, testLintTwoAlternatives
> 	, testLintResultNonempty
> 	, testRangeFromLargerThanTo
> 	]

