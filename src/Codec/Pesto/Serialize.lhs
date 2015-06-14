Serializing
-----------

.. class:: nodoc

> module Codec.Pesto.Serialize (serialize) where
> import Data.Char (isSpace, toLower, isLetter)
> import Data.Ratio (numerator, denominator)
>
> import {-# SOURCE #-} Codec.Pesto.Parse

> class Serializeable a where
> 	serialize :: a -> String

.. class:: todo

- Add instance for graph
- use :math:`\mathcal{O}(1)` string builder


Finally transform linear stream of operations into a string again:

> instance Serializeable a => Serializeable [a] where
> 	serialize ops = unlines $ map serialize ops

> instance Serializeable Operation where
> 	serialize (Annotation s) = quote '(' ')' s
> 	serialize (Ingredient q) = '+':serialize q
> 	serialize (Tool q) = '&':serialize q
> 	serialize (Action s) = quote '[' ']' s
> 	serialize (Reference q) = '*':serialize q
> 	serialize (Result s) = '>':serializeQstr s
> 	serialize (Alternative s) = '|':serializeQstr s

> instance Serializeable Quantity where
> 	serialize (Quantity a b "") = serialize a ++ " " ++ serializeQstr b
> 	serialize (Quantity (Exact (AmountStr "")) "" c) = serializeQstr c
> 	serialize (Quantity a "" c) = serialize a ++ " _ " ++ serializeQstr c
> 	serialize (Quantity a b c) = serialize a ++ " " ++ serializeQstr b ++ " " ++ serializeQstr c

> instance Serializeable Approximately where
> 	serialize (Range a b) = serialize a ++ "-" ++ serialize b
> 	serialize (Approx a) = '~':serialize a
> 	serialize (Exact a) = serialize a

There are two special cases here, both for aesthetic reasons:

1) If the denominator is one we can just skip printing it, because
   :math:`\frac{2}{1} = 2` and
2) if the numerator is larger than the denominator use mixed fraction notation,
   because :math:`\frac{7}{2} = 3+\frac{1}{2}`

> instance Serializeable Amount where
> 	serialize (AmountRatio a) | denominator a == 1 = show (numerator a)
> 	serialize (AmountRatio a) | numerator a > denominator a =
> 		show full ++ "/" ++ show num ++ "/" ++ show denom
> 		where
> 			full = numerator a `div` denom
> 			num = numerator a - full * denom
> 			denom = denominator a
> 	serialize (AmountRatio a) = show (numerator a) ++ "/" ++ show (denominator a)
> 	serialize (AmountStr s) = serializeQstr s

> serializeQstr "" = "_"
> serializeQstr s | (not . isLetter . head) s || hasSpaces s = quote '"' '"' s
> serializeQstr s = s
> hasSpaces = any isSpace
> quote start end s = [start] ++ concatMap (\c -> if c == end then ['\\', end] else [c]) s ++ [end]

