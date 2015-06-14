Language syntax
---------------

.. class:: nodoc

> module Codec.Pesto.Parse (
> 	  parse
> 	, test
> 	, Operation(..)
> 	, Quantity(..)
> 	, Unit(..)
> 	, Object(..)
> 	, Approximately(..)
> 	, Amount(..)
> 	, Recipe(..)
> 	, isResult
> 	, isReference
> 	, isAlternative
> 	, isAnnotation
> 	, isAction
> 	, spaces1
> 	, notspace
> 	) where
> import Control.Applicative ((<*>), (<$>), (<*), (*>))
> import Data.Char (isSpace, toLower, isLetter)
> import Data.Ratio ((%))
> import Text.Parsec hiding (parse)
> import Text.Parsec.Char
> import Text.ParserCombinators.Parsec.Pos (newPos)
> import Text.ParserCombinators.Parsec.Error (ParseError, Message,
> 		errorMessages, messageEq, newErrorUnknown)
> import Test.HUnit hiding (test)
>
> import Codec.Pesto.Serialize (serialize)

XXX: magic should be an operation
XXX: this parser should accept invalid operations

From the XXXsyntactic point of view a Pesto recipe is just a list of
space-delimited operations. It is encoded with UTF-8_ and starts with a magic
identifier (``%pesto-1``) followed by one or more spaces (spaces1_). Every
character within the Unicode whitespace class is considered a space.

.. _UTF-8: https://tools.ietf.org/html/rfc3629

.. _spaces1:
.. _Recipe:

> data Recipe = Recipe {
> 	  version :: Integer
> 	, operations :: [(SourcePos, Operation)]
> 	} deriving Show
> 
> recipe = Recipe
> 	<$> magic <* spaces1
> 	<*> ((,) <$> getPosition <*> operation) `sepEndBy` spaces1
> 	<*  eof
> 	<?> "recipe"
> 
> spaces1 = many1 space

The file identifier consists of the string ``%pesto-`` followed by an integral
number and arbitrary non-space characters. They are reserved for future use and
must be ignored by parsers implementing this version of pesto. A byte order
mark (BOM) must not be used.

> magic = string "%pesto-" *> int <* skipMany notspace <?> "magic"
> notspace = satisfy (not . isSpace)

.. _Operation:
.. _Ingredient:
.. _Tool:
.. _Result:
.. _Alternative:
.. _Reference:
.. _Annotation:
.. _Action:

The following *operations* are supported:

> data Operation =
> 	  Annotation String
> 	| Ingredient Quantity
> 	| Tool Quantity
> 	| Action String
> 	| Reference Quantity
> 	| Result Object
> 	| Alternative Object
> 	deriving (Show, Eq)
>
> operation =
> 	    try annotation
> 	<|> try ingredient
> 	<|> try tool
> 	<|> try action
> 	<|> try result
> 	<|> try alternative
> 	<|> try reference
> 	<?> "operation"

The pesto grammar has two kinds of operations: The first one begins with a
start character and consumes characters up to and including a terminating
symbol (``end``), which can be escaped with a backslash (``\``):

> betweenEscaped start end =
> 	   char start
> 	*> many (try (char '\\' *> char end) <|> satisfy (/= end))
> 	<* char end

Annotations and Actions both are of this kind:

> annotation = Annotation <$> betweenEscaped '(' ')'
> action = Action <$> betweenEscaped '[' ']'

Here are examples for both:

> testOpterm = [cmpOperation "(skinless\nboneless)" (Right (Annotation "skinless\nboneless"))
> 	, cmpOperation "[stir together]" (Right (Action "stir together"))
> 	, cmpOperation "[stir\\]together]" (Right (Action "stir]together"))]


The second one starts with one identifying character, ignores the following
whitespace characters and then consumes an object or a quantity.

> oparg ident cont = char ident *> spaces *> cont
> ingredient = oparg '+' (Ingredient <$> quantity)
> tool = oparg '&' (Tool <$> quantity)
> result = oparg '>' (Result <$> object)
> alternative = oparg '|' (Alternative <$> object)
> reference = oparg '*' (Reference <$> quantity)

> testOparg = [
> 	  cmpOperation "+100 g flour" (Right (Ingredient (Quantity (Exact (AmountRatio (100%1))) "g" "flour")))

> 	, cmpOperation "&oven" (Right (Tool (Quantity (Exact (AmountStr "")) "" "oven")))
> 	, cmpOperation ">dough" (Right (Result "dough"))
> 	, cmpOperation "|trimmings" (Right (Alternative "trimmings"))
> 	, cmpOperation "*fish" (Right (Reference (Quantity (Exact (AmountStr "")) "" "fish")))
> 	, cmpOperation3 "* \t\n 1 _ cheese" (Right (Reference (Quantity (Exact (AmountRatio (1%1))) "" "cheese"))) "*1 _ cheese"
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

The operations Ingredient, Tool and Reference accept a *quantity*, that is a
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
> parse = runParser recipe () ""

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

> cmpOperation a b = cmpOperation3 a b a
> cmpOperation3 = cmpParseSerialize operation

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

