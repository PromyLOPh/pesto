=========================
Pesto specification draft
=========================

Pesto is a text-based human-editable and machine-transformable cooking recipe
interchange format.

.. class:: nodoc

> module Codec.Pesto where

About this document
-------------------

This section contains various information about this document. The `second
section <motivation_>`_ motivates why inventing another file format is
necessary, followed by the goals_ of Pesto. After a short Pesto `primer
<introduction-by-example_>`_ intended for the casual user the language’s
`syntax <language-syntax_>`_ and `semantics <language-semantics_>`_ are
presented. The `linting section <linting_>`_ limits the language to useful
cooking recipes.  Examples for user presentation of recipes and serialization
follow.

Being a literate program this document is specification and reference
implementation at the same time. The code is written in Haskell_ and uses the
parsec_ parser combinator library, as well as HUnit_ for unit tests. Even
without knowing Haskell’s syntax you should be able to understand this
specification. There’s a description above every code snippet explaining what
is going on.

.. _Haskell: http://learnyouahaskell.com/
.. _HUnit: http://hackage.haskell.org/package/HUnit
.. _parsec: http://hackage.haskell.org/package/parsec

The key words “MUST”, “MUST NOT”, “REQUIRED”, “SHALL”, “SHALL NOT”, “SHOULD”,
“SHOULD NOT”, “RECOMMENDED”,  “MAY”, and “OPTIONAL” in this document are to be
interpreted as described in `RFC 2119`_.

.. _RFC 2119: http://tools.ietf.org/html/rfc2119

:Version: 1-draft
:License: CC0_
:Website: https://6xq.net/pesto/
:Discussion: https://github.com/PromyLOPh/pesto
:Contributors:
	- `Lars-Dominik Braun <mailto:lars+pesto@6xq.net>`_

.. _CC0: https://creativecommons.org/publicdomain/zero/1.0/

.. _motivation:

Motivation
----------

The landscape of recipe interchange formats is quite fragmented. First of all
there’s HTML microdata. `Google rich snippets`_, which are equivalent to the
schema.org_ microdata vocabulary, are widely used by commercial recipe sites.
Although the main objective of microdata is to make content machine-readable
most sites will probably do so, because it is considered a search-engine
optimization (SEO). Additionally parsing HTML pulled from the web is a
nightmare and thus not a real option for sharing recipes. h-recipe_ provides a
second vocabulary, but has not been adopted widely.

.. _Google rich snippets: https://developers.google.com/structured-data/rich-snippets/recipes
.. _schema.org: http://schema.org/Recipe
.. _h-recipe: http://microformats.org/wiki/h-recipe

Mealmaster_, an ancient file format with – due to its age – many problems,
seems to have the most traction right now. A large amount of recipe files is
`available in this format <http://www.ffts.com/recipes.htm>`_. Rezkonv_ aims to
improve the Mealmaster format. However the specification is available on
request only. Another text-based format, MXP_ (plus MX2, MZ2), is used by
Mastercook.

A newer format, YumML_, is based on YAML. The specification has been removed
from the web and is available through the Web Archive only.

.. _xml-formats:

There’s a number of XML-based formats:
RecipeML_, formerly known as DESSERT was released in 2002 (version 0.5). The
license requires attribution and – at the same time – forbids using the name
RecipeML for promotion without written permission.
REML_ was released in 2005 (version 0.5). It is rather complicated and has no
license.
`RecipeBook XML`_, released in 2005 as well and shared unter the terms of `CC
by-sa`_ is not available on the web any more.
CookML_, created in 2006 (version 1.0.4) for the german-language shareware
program Kalorio has a custom and restrictive licence that requires attribution and
forbids derivate works.
KRecipes_ uses its own export format. However there is no documentation
whatsoever. Gourmet_’s export format suffers from the same problem. The only
document available is the `DTD
<https://github.com/thinkle/gourmet/blob/7715c6ef87ee8c106f0a021972cd70d61d83cadb/data/recipe.dtd>`_.

.. _CC by-sa: https://creativecommons.org/licenses/by-sa/2.5/

All of the formats above share a common design aspect: They split recipes into
two parts, ingredients and instructions. This is quite odd given the nature of
cooking recipes. RxOL_, created in 1985, represents recipes as a graph with
postfix notation and minimal “chitchat”. Although Pesto is not a direct
descendant of RxOL it’s syntax and semantics are quite similar.

.. _REML: http://reml.sourceforge.net/
.. _RecipeML: http://www.formatdata.com/recipeml/index.html
.. _CookML: http://www.kalorio.de/index.php?Mod=Ac&Cap=CE&SCa=../cml/CookML_EN
.. _Mealmaster: http://www.wedesoft.de/anymeal-api/mealmaster.html
.. _MXP: http://www.oocities.org/heartland/woods/2073/Appendix.htm
.. _RecipeBook XML: http://web.archive.org/web/20141101132332/http://www.happy-monkey.net/recipebook/
.. _YumML: http://web.archive.org/web/20140703234140/http://vikingco.de/yumml.html
.. _Rezkonv: http://www.rezkonv.de/software/rksuite/rkformat.html
.. _RxOL: http://www.dodomagnifico.com/641/Recipes/CompCook.html
.. _Gourmet: http://thinkle.github.io/gourmet/
.. _KRecipes: http://krecipes.sourceforge.net/

.. There is a copy at http://diyhpl.us/~bryan/papers2/CompCook.html as well

.. More interesting stuff:
.. - http://blog.moertel.com/posts/2010-01-08-a-formal-language-for-recipes-brain-dump.html
.. - http://www.dangermouse.net/esoteric/chef.html

Goals
-----

First of all recipes are written *by* humans *for* humans. Thus a
human-readable recipe interchange format is not enough. The recipes need to be
human-editable without guidance like a GUI or assistant. That’s why, for
instance, XML is not suitable and the interchange formats listed `above
<xml-formats_>`_ have largely failed to gain traction. XML, even though simple
itself, is still too complicated for the ordinary user. Instead a format needs
to be as simple as possible, with as little markup as possible. A human editor
must be able to remember the entire syntax. This works best if the file
contents “make sense”. A good example for this is Markdown_.

.. _Markdown: https://daringfireball.net/projects/markdown/syntax

We also have to acknowledge that machines play an important role in our daily
life. They can help us, the users, accomplish our goals if they are able to
understand the recipes as well. Thus they too need to be able to read and write
recipes. Again, designing a machine-readable format is not enough. Recipes must
be machine-transformable. A computer program should be able to create a new
recipe from two existing ones, look up the ingredients and tell us how many
joules one piece of that cake will have. And so on.

That being said, Pesto does not aim to carry additional information about
ingredients or recipes itself. Nutrition data for each ingredient should be
maintained in a separate database. Due to its minimal syntax Pesto is also not
suitable for extensive guides on cooking or the usual chitchat found in cooking
books.

Introduction by example
-----------------------

So let’s start by introducing Pesto in a XXXnon-formal, XXX way: By example. We
are now going to cook XXX. The following recipe contains all the information
you need to do that.

.. class:: todo

do it.

See https://github.com/PromyLOPh/rezepte for example recipes.

.. include:: Pesto/Parse.lhs
.. include:: Pesto/Graph.lhs
.. include:: Pesto/Lint.lhs
.. include:: Pesto/Dot.lhs
.. include:: Pesto/Serialize.lhs

Using this project
------------------

This project uses cabal. It provides the Codec.Pesto library that implements
the Pesto language as described in the previous sections. It also comes with
three binaries.

.. include:: ../Main.lhs
.. include:: ../Test.lhs
.. include:: ../Doc.lhs

Final words
-----------

.. class:: todo

Do we even need this?

