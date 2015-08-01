Language syntax
---------------

.. class:: nodoc

> module Codec.Pesto.Parse (
> 	  parse
> 	, test
> 	, Instruction(..)
> 	, Quantity(..)
> 	, Unit
> 	, Object
> 	, Approximately(..)
> 	, Amount(..)
> 	, isResult
> 	, isReference
> 	, isAlternative
> 	, isAnnotation
> 	, isAction
> 	, isDirective
> 	, isUnknown
> 	, spaces1
> 	, notspace
> 	) where
> import Control.Applicative ((<*>), (<$>), (<*), (*>))
> import Data.Char (isSpace)
> import Data.Ratio ((%))
> import Text.Parsec hiding (parse)
> import Text.ParserCombinators.Parsec.Pos (newPos)
> import Text.ParserCombinators.Parsec.Error (newErrorUnknown)
> import Test.HUnit hiding (test)
>
> import Codec.Pesto.Serialize (serialize)

Pesto parses UTF-8_ encoded input data consisting of space-delimited
instructions.  Every character within the Unicode whitespace class is
considered a space.

.. _UTF-8: https://tools.ietf.org/html/rfc3629
.. _spaces1:

> stream = ((,) <$> getPosition <*> instruction) `sepEndBy` spaces1
> 	<?> "stream"
> spaces1 = many1 space

The following instructions are supported:

> data Instruction =
> 	  Annotation String
> 	| Ingredient Quantity
> 	| Tool Quantity
> 	| Action String
> 	| Reference Quantity
> 	| Result Object
> 	| Alternative Object
> 	| Directive String
> 	| Unknown String
> 	deriving (Show, Eq)
>
> instruction =
> 	    try annotation
> 	<|> try ingredient
> 	<|> try tool
> 	<|> try action
> 	<|> try result
> 	<|> try alternative
> 	<|> try reference
> 	<|> try directive
> 	<|> try unknown
> 	<?> "instruction"

The pesto grammar has two instruction types: The first one begins with a
start symbol (``start``) and consumes any character up to and including a
terminating symbol (``end``), which can be escaped with a backslash (``\``).

> betweenEscaped start end =
> 	   char start
> 	*> many (try (char '\\' *> char end) <|> satisfy (/= end))
> 	<* char end

Annotations and actions both are of this kind:

> annotation = Annotation <$> betweenEscaped '(' ')'
> action = Action <$> betweenEscaped '[' ']'

Here are examples for both:

> testOpterm = [cmpInstruction "(skinless\nboneless)" (Right (Annotation "skinless\nboneless"))
> 	, cmpInstruction "[stir together]" (Right (Action "stir together"))
> 	, cmpInstruction "[stir\\]together]" (Right (Action "stir]together"))]

The second one starts with one identifying character, ignores the following
whitespace characters and then consumes an object or a quantity.

> oparg ident cont = char ident *> spaces *> cont
> ingredient = oparg '+' (Ingredient <$> quantity)
> tool = oparg '&' (Tool <$> quantity)
> result = oparg '>' (Result <$> object)
> alternative = oparg '|' (Alternative <$> object)
> reference = oparg '*' (Reference <$> quantity)

Additionally there are two special instructions. Directives are similar to the
previous instructions, but consume a qstr.

> directive = oparg '%' (Directive <$> qstr)

Unknown instructions are the fallthrough-case and accept anything. They must
not be discarded at this point. The point of accepting anything is to fail as
late as possible while processing input. This gives the parser a chance to
print helpful mesages that provide additional aid to the user who can then fix
the problem.

> unknown = Unknown <$> many1 notspace

Below are examples for these instructions:

> testOparg = [
> 	  cmpInstruction "+100 g flour"
> 	      (Right (Ingredient (Quantity (Exact (AmountRatio (100%1))) "g" "flour")))
> 	, cmpInstruction "&oven"
> 	      (Right (Tool (Quantity (Exact (AmountStr "")) "" "oven")))
> 	, cmpInstruction ">dough" (Right (Result "dough"))
> 	, cmpInstruction "|trimmings" (Right (Alternative "trimmings"))
> 	, cmpInstruction "*fish"
> 	      (Right (Reference (Quantity (Exact (AmountStr "")) "" "fish")))
> 	, cmpInstruction3 "% invalid" (Right (Directive "invalid")) "%invalid"
> 	, cmpInstruction3 "* \t\n 1 _ cheese"
> 	      (Right (Reference (Quantity (Exact (AmountRatio (1%1))) "" "cheese")))
> 	      "*1 _ cheese"
> 	]

Qstr
++++

Before introducing quantities we need to have a look at qstr, which is used by
them. A qstr, short for quoted string, can be – you guessed it already – a
string enclosed in double quotes, a single word or the underscore character
that represents the empty string.

> qstr = try (betweenEscaped '"' '"')
> 	<|> word
> 	<|> char '_' *> return ""
 
A word always starts with a letter, followed by any number of non-space
characters.

> word = (:) <$> letter <*> many notspace
> notspace = satisfy (not . isSpace)

The empty string can be represented by two double quotes or the underscore, but
not the empty string itself.

> testQstr = [
> 	  cmpQstr3 "\"\"" (Right "") "_"
> 	, cmpQstr "_" (Right "")
> 	, cmpQstr "" parseError

Any Unicode character with a General_Category major class L (i.e. a letter, see
`Unicode standard section 4.5
<http://www.unicode.org/versions/Unicode7.0.0/ch04.pdf>`_ for example) is
accected as first character of a word. That includes german umlauts as well as
greek or arabic script. Numbers, separators, punctuation and others are not
permitted.

> 	, cmpQstr "water" (Right "water")
> 	, cmpQstr "Äpfel" (Right "Äpfel")
> 	, cmpQstr "τυρί" (Right "τυρί")
> 	, cmpQstr "جبن" (Right "جبن")
> 	, cmpQstr "1sugar" parseError
> 	, cmpQstr "+milk" parseError
> 	, cmpQstr "∀onion" parseError

The remaining letters of a word can be any character, including symbols,
numbers, …

> 	, cmpQstr "rump-roast" (Right "rump-roast")
> 	, cmpQstr "v1negar" (Right "v1negar")
> 	, cmpQstr "mush\"rooms" (Right "mush\"rooms")

…but not spaces.

> 	, cmpQstr " tomatoes" parseError
> 	, cmpQstr "tomatoes " parseError
> 	, cmpQstr "lemon juice" parseError
> 	, cmpQstr "sour\tcream" parseError
> 	, cmpQstr "white\nwine" parseError

If a string contains spaces or starts with a special character it must be
enclosed in double quotes.

> 	, cmpQstr3 "\"salt\"" (Right "salt") "salt"
> 	, cmpQstr "\"+milk\"" (Right "+milk")
> 	, cmpQstr "\"soy sauce\"" (Right "soy sauce")
> 	, cmpQstr "\"1sugar\"" (Right "1sugar")
> 	, cmpQstr "\"chicken\tbreast\nmeat\"" (Right "chicken\tbreast\nmeat")

Double quotes within a string can be quoted by prepending a backslash. However
the usual escape codes like \\n, \\t, … will *not* be expanded.

> 	, cmpQstr "\"vine\"gar\"" parseError
> 	, cmpQstr3 "\"vine\\\"gar\"" (Right "vine\"gar") "vine\"gar"
> 	, cmpQstr "\"oli\\ve oil\"" (Right "oli\\ve oil")
> 	, cmpQstr "\"oli\\\\\"ve oil\"" (Right "oli\\\"ve oil")
> 	, cmpQstr3 "\"sal\\tmon\"" (Right "sal\\tmon") "sal\\tmon"
>	]

Quantity
++++++++

The instructions Ingredient, Tool and Reference accept a *quantity*, that is a
triple of Approximately, Unit and Object as parameter.

> data Quantity = Quantity Approximately Unit Object deriving (Show, Eq)

The syntactic construct is overloaded and accepts one to three arguments. If
just one is given it is assumed to be the Object and Approximately and Unit are
empty. Two arguments set Approximately and Unit, which is convenient when the
unit implies the object (minutes usually refer to the object time, for
example).

> quantity = try quantityA <|> quantityB

> quantityA = Quantity
> 	<$> approximately
> 	<*  spaces1
> 	<*> unit
> 	<*> (try (spaces1 *> object) <|> return "")

> quantityB = Quantity
> 	<$> return (Exact (AmountStr ""))
> 	<*> return ""
> 	<*> object

> testQuantityOverloaded = [
> 	  cmpQuantity "oven" (exactQuantity (AmountStr "") "" "oven")
> 	, cmpQuantity "10 min" (exactQuantity (AmountRatio (10%1)) "min" "")
> 	, cmpQuantity "100 g flour" (exactQuantity (AmountRatio (100%1)) "g" "flour")

The first two are equivalent to

> 	, cmpQuantity3 "_ _ oven" (exactQuantity (AmountStr "") "" "oven") "oven"
> 	, cmpQuantity3 "10 min _" (exactQuantity (AmountRatio (10%1)) "min" "") "10 min"

Missing units must not be ommited. The version with underscore should be prefered.

>	, cmpQuantity3 "1 \"\" meal" (exactQuantity (AmountRatio (1%1)) "" "meal") "1 _ meal"
>	, cmpQuantity "1 _ meal" (exactQuantity (AmountRatio (1%1)) "" "meal")
>	]

Units and objects are just strings. However units should be limited to
`well-known metric units <well-known-units_>`_ and `some guidelines
<objects-and-annotations_>`_ apply to Objects as well.

> type Unit = String
> unit = qstr
>
> type Object = String
> object = qstr

Approximately is a wrapper for ranges, that is two amounts separated by a dash,
approximate amounts, prepended with a tilde and exact amounts without modifier.

> data Approximately =
> 	  Range Amount Amount
> 	| Approx Amount
> 	| Exact Amount
> 	deriving (Show, Eq)
> 
> approximately = try range <|> try approx <|> exact
> range = Range <$> amount <*> (char '-' *> amount)
> approx = Approx <$> (char '~' *> amount)
> exact = Exact <$> amount

> testQuantityApprox = [
> 	  cmpQuantity "1-2 _ bananas" (Right (Quantity (Range (AmountRatio (1%1)) (AmountRatio (2%1))) "" "bananas"))
> 	, cmpQuantity "1 - 2 _ bananas" parseError
> 	, cmpQuantity "1- 2 _ bananas" parseError
> 	, cmpQuantity "1 -2 _ bananas" parseError
> 	, cmpQuantity "~2 _ bananas" (Right (Quantity (Approx (AmountRatio (2%1))) "" "bananas"))
> 	, cmpQuantity "~ 2 _ bananas" parseError

>	]

Amounts are limited to rational numbers and strings. There are no real numbers
by design and implementations should avoid representing rational numbers as
IEEE float. They are not required and introduce ugly corner cases when
rounding while converting units for example.

> data Amount =
> 	  AmountRatio Rational
> 	| AmountStr String
> 	deriving (Show, Eq)
>
> amount = try ratio <|> AmountStr <$> qstr

> testQuantityAmount = [
> 	  cmpQuantity "some _ pepper" (exactQuantity (AmountStr "some") "" "pepper")
> 	, cmpQuantity3 "\"some\"-\"a few\" _ bananas" (Right (Quantity (Range (AmountStr "some") (AmountStr "a few")) "" "bananas")) "some-\"a few\" _ bananas"
> 	, cmpQuantity "~\"the stars in your eyes\" _ bananas" (Right (Quantity (Approx (AmountStr "the stars in your eyes")) "" "bananas"))
> 	]

Rational numbers can be an integral, numerator and denominator, each separated
by a forward slash, just the numerator and denominator, again separated by a
forward slash or just a numerator with the default denominator 1 (i.e. ordinary
integral number).

> ratio = let toRatio i num denom = AmountRatio ((i*denom+num)%denom) in
> 	    try (toRatio <$> int <*> (char '/' *> int) <*> (char '/' *> int))
> 	<|> try (toRatio <$> return 0 <*> int <*> (char '/' *> int))
> 	<|> try (toRatio <$> return 0 <*> int <*> return 1)

These are all equal.

> testQuantityRatio = [
> 	  cmpQuantity "3 _ bananas" (exactQuantity (AmountRatio (3%1)) "" "bananas")
> 	, cmpQuantity3 "3/1 _ bananas" (exactQuantity (AmountRatio (3%1)) "" "bananas") "3 _ bananas"
> 	, cmpQuantity3 "3/0/1 _ bananas" (exactQuantity (AmountRatio (3%1)) "" "bananas") "3 _ bananas"

XXtwo is num and denom

> 	, cmpQuantity "3/5 _ bananas" (exactQuantity (AmountRatio (3%5)) "" "bananas")

three is int, num and denom

> 	, cmpQuantity "3/5/7 _ bananas" (exactQuantity (AmountRatio ((3*7+5)%7)) "" "bananas")

> 	, cmpQuantity3 "10/3 _ bananas" (exactQuantity (AmountRatio (10%3)) "" "bananas") "3/1/3 _ bananas"

Can be used with ranges and approximate too. and mixed with strings

> 	, cmpQuantity "1-\"a few\" _ bananas" (Right (Quantity (Range (AmountRatio (1%1)) (AmountStr "a few")) "" "bananas"))
> 	, cmpQuantity "1/1/2-2 _ bananas" (Right (Quantity (Range (AmountRatio (3%2)) (AmountRatio (4%2))) "" "bananas"))
> 	, cmpQuantity "~1/1/2 _ bananas" (Right (Quantity (Approx (AmountRatio (3%2))) "" "bananas"))

> 	]

Appendix
++++++++

> int = read <$> many1 digit
> parse = runParser stream () ""

Test helpers:

> isLeft (Left _) = True
> isLeft _ = False

> parseError = Left (newErrorUnknown (newPos "" 0 0))
> cmpParser f str (Left _) = TestCase $ assertBool str $ isLeft $ runParser (f <* eof) () "" str
> cmpParser f str expected = str ~: runParser (f <* eof) () "" str ~?= expected

> cmpParseSerialize f str expectp@(Left _) _ = [cmpParser f str expectp]
> cmpParseSerialize f str expectp@(Right expectpval) expects = [
> 	  cmpParser f str expectp
> 	, serialize expectpval ~?= expects]

Wrap qstr test in AmountStr to aid serialization test

> cmpQstr input expectp = cmpQstr3 input expectp input
> cmpQstr3 input (Left expect) _ = [cmpParser (AmountStr <$> qstr) input (Left expect)]
> cmpQstr3 input (Right expect) expects = cmpParseSerialize (AmountStr <$> qstr) input (Right (AmountStr expect)) expects

> cmpQuantity a b = cmpQuantity3 a b a
> cmpQuantity3 = cmpParseSerialize quantity

> cmpInstruction a b = cmpInstruction3 a b a
> cmpInstruction3 = cmpParseSerialize instruction

> exactQuantity a b c = Right (Quantity (Exact a) b c)

> test = [
> 	  "quantity" ~: testQuantityOverloaded ++ testQuantityApprox ++ testQuantityAmount ++ testQuantityRatio
>	, "qstr" ~: testQstr
>	, "oparg" ~: testOparg
> 	, "opterm" ~: testOpterm
> 	]

> isResult (Result _) = True
> isResult _ = False
> isReference (Reference _) = True
> isReference _ = False
> isAlternative (Alternative _) = True
> isAlternative _ = False
> isAnnotation (Annotation _) = True
> isAnnotation _ = False
> isAction (Action _) = True
> isAction _ = False
> isDirective (Directive _) = True
> isDirective _ = False
> isUnknown (Unknown _) = True
> isUnknown _ = False

