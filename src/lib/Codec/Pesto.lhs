=========================
Pesto specification draft
=========================

Pesto is a text-based human-editable and machine-transformable cooking recipe
interchange format.

.. warning::

	This specification is work-in-progress and thus neither stable, consistent or
	complete.

.. class:: nodoc

> module Codec.Pesto where

About this document
-------------------

This section contains various information about this document. The `second
section`__ motivates why inventing another file format is necessary, followed
by the goals__ of Pesto. After a short Pesto primer__ intended for the casual
user the language’s syntax__ and semantics__ are presented. The `linting
section`__ limits the language to useful cooking recipes.  Examples for user
presentation of recipes and serialization follow.

__ #motivation
__ #goals
__ #introduction-by-example
__ #language-syntax
__ #language-semantics
__ #linting

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
most sites will probably use it, because it is considered a search-engine
optimization (SEO). Additionally parsing HTML pulled from the web is a
nightmare and thus not a real option for sharing recipes. h-recipe_ provides a
second vocabulary that has not been adopted widely yet.

.. _Google rich snippets: https://developers.google.com/structured-data/rich-snippets/recipes
.. _schema.org: http://schema.org/Recipe
.. _h-recipe: http://microformats.org/wiki/h-recipe

.. _formats-by-software:

Most cooking-related software comes with its own recipe file format. Some of
them, due to their age, can be imported by other programs.

Meal-Master_ is one of these widely supported formats. A huge trove of recipe files
is `available in this format <http://www.ffts.com/recipes.htm>`_. There does
not seem to be any official documentation for the format, but inofficial
`ABNF grammar`_ and `format description <http://www.ffts.com/mmformat.txt>`_
exist. A Meal-Master recipe template might look like this:

.. _MasterCook: http://mastercook.com/
.. _MXP: http://www.oocities.org/heartland/woods/2073/Appendix.htm
.. _ABNF grammar: http://web.archive.org/web/20161002135718/http://www.wedesoft.de/anymeal-api/mealmaster.html

.. code:: mealmaster

    ---------- Recipe via Meal-Master (tm)

          Title: <Title>
     Categories: <Categories>
          Yield: <N servings>

        <N> <unit> <ingredient>
        …

    -------------------------------<Section name>-----------------------------
      <More ingredients>

      <Instructions>

    -----

Rezkonv_ aims to improve the Mealmaster format by lifting some of its character
limits, adding new syntax and translating it to german. However the
specification is available on request only.

A second format some programs can import is MasterCook_’s MXP_ file format, as
well as its XML-based successor MX2. And then there’s a whole bunch of
more-or-less proprietary formats:

`Living Cookbook`_
	Uses a XML-based format called fdx version 1.1. There’s no specification to
	be found, but a few examples__ are available and those are dated 2006.
`My CookBook`_
	Uses the file extension .mcb. A specification `is available
	<http://mycookbook-android.com/site/my-cookbook-xml-schema/>`_.
KRecipes_
	Uses its own export format. However there is no documentation whatsoever.
Gourmet_
	The program’s export format suffers from the same problem. The only
	document available is the `DTD
	<https://github.com/thinkle/gourmet/blob/7715c6ef87ee8c106f0a021972cd70d61d83cadb/data/recipe.dtd>`_.
CookML_
	Last updated in 2006 (version 1.0.4) for the german-language shareware
	program Kalorio has a custom and restrictive licence that requires
	attribution and forbids derivate works.
Paprika_
	Cross-platform application, supports its own “emailed recipe format” and a
	simple YAML-based format.

__ http://livingcookbook.com/Resource/DownloadableRecipes
.. _Paprika: https://paprikaapp.com/help/android/#importrecipes

.. _xml-formats:

Between 2002 and 2005 a bunch of XML-based exchange formats were created. They
are not tied to a specific software, so none of them seems to be actively used
nowadays:

RecipeML_
	Formerly known as DESSERT and released in 2002 (version 0.5). The
	license requires attribution and – at the same time – forbids using the name
	RecipeML for promotion without written permission.
eatdrinkfeelgood_
	Version 1.1 was released in 2002 as well, but the site is not online
	anymore. The DTD is licensed under the `CC by-sa`_ license.
REML_
	Released in 2005 (version 0.5), aims to improve support for commercial uses
	(restaurant menus and cookbooks). The XSD’s license permits free use and
	redistribution, but the reference implementation has no licensing
	information.
`RecipeBook XML`_
	Released 2005 as well and shared unter the terms of `CC by-sa`_ is not
	available on the web any more.

.. _CC by-sa: https://creativecommons.org/licenses/by-sa/2.5/

.. _obscure-formats:

Finally, a few non-XML or obscure exchange formats have been created in the past:
YumML_ is an approach similar to those listed above, but based on YAML instead
of XML. The specification has been removed from the web and is available
through the Web Archive only.

`Cordon Bleu`_ (1999) encodes recipes as programs for a cooking machine and
defines a Pascal-like language. Being so close to real programming languages
Cordon Bleu is barely useable by anyone except programmers. Additionally the
language is poorly-designed, since its syntax is inconsistent and the user is
limited to a set of predefined functions.

Finally there is RxOL_, created in 1985. It constructs a graph from recipes
written down with a few operators and postfix notation. It does not separate
ingredients and cooking instructions like every other syntax introduced before.
Although Pesto is not a direct descendant of RxOL both share many ideas.

microformats.org_ has a similar list of recipe interchange formats.

.. _REML: http://reml.sourceforge.net/
.. _eatdrinkfeelgood: https://web.archive.org/web/20070109085643/http://eatdrinkfeelgood.org/1.1/
.. _RecipeML: http://www.formatdata.com/recipeml/index.html
.. _CookML: http://www.kalorio.de/index.php?Mod=Ac&Cap=CE&SCa=../cml/CookML_EN
.. _Meal-Master: http://web.archive.org/web/20151029032924/http://episoft.home.comcast.net:80/~episoft/
.. _RecipeBook XML: http://web.archive.org/web/20141101132332/http://www.happy-monkey.net/recipebook/
.. _YumML: http://web.archive.org/web/20140703234140/http://vikingco.de/yumml.html
.. _Rezkonv: http://www.rezkonv.de/software/rksuite/rkformat.html
.. _RxOL: http://web.archive.org/web/20150814041516/www.dodomagnifico.com/641/Recipes/CompCook.html
.. _Gourmet: http://thinkle.github.io/gourmet/
.. _KRecipes: http://krecipes.sourceforge.net/
.. _Cordon Bleu: http://web.archive.org/web/20090115210732/http://www.inf.unideb.hu/~bognar/ps_ek/cb_lang.ps
.. _microformats.org: http://microformats.org/wiki/recipe-formats
.. _Living Cookbook: http://livingcookbook.com/
.. _My CookBook: http://mycookbook-android.com/

.. There is a copy at http://diyhpl.us/~bryan/papers2/CompCook.html as well

.. More interesting stuff:
.. - http://blog.moertel.com/posts/2010-01-08-a-formal-language-for-recipes-brain-dump.html
.. - http://www.dangermouse.net/esoteric/chef.html

.. _goals:

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

.. _introduction-by-example:

Introduction by example
-----------------------

.. code::

	So let’s start by introducing Pesto by example. This text does not belong
	to the recipe and is ignored by any software. The following line starts the
	recipe:

	%pesto

	&pot
	+1 l water
	+salt
	[boil]

	+100 g penne
	&10 min
	[cook]

	>1 serving pasta
	(language: en)

And that’s how you make pasta: Boil one liter of water in a pot with a little
bit of salt. Then add 100 g penne, cook them for ten minutes and you get one
serving pasta. That’s all.

There’s more syntax available to express alternatives (either penne or
tagliatelle), ranges (1–2 l water or approximately 1 liter water) and metadata.
But now you can have a first peek at `my own recipe collection`_.

.. _my own recipe collection: https://github.com/PromyLOPh/rezepte

.. include:: Pesto/Parse.lhs
.. include:: Pesto/Graph.lhs
.. include:: Pesto/Lint.lhs
.. include:: Pesto/Serialize.lhs

Using this project
------------------

This project uses cabal. It provides the Codec.Pesto library that implements
the Pesto language as described in the previous sections. It also comes with
three binaries.

.. include:: ../../exe/Main.lhs
.. include:: ../../exe/Test.lhs
.. include:: ../../exe/Doc.lhs

