Presentation
------------

.. class:: nodoc

> module Codec.Pesto.Dot (toDot) where
> import Codec.Pesto.Serialize (serialize)

Since each recipe is just a directed graph (digraph), we can use the dot
language to represent it as well. This in turnXXX can be transformed into an
image, for example.

> toDot nodes edges = unlines $ ["digraph a {\nnode [fontname=\"Roboto Semi-Light\"];"] ++ n ++ e ++ ["}"]
> 	where
>		f (a, b) = show a ++ " -> " ++ show b ++ ";"
> 		e = map f edges
> 		n = map (\(a, b) -> show a ++ " [label=\"" ++ dotEncodeString (serialize b) ++ "\"];") nodes
>		addcolor = "#e6ee9c"

> dotEncodeString = concatMap dotEncodeChar
> dotEncodeChar '\n' = "\\n"
> dotEncodeChar '"' = "\\\""
> dotEncodeChar x = [x]

