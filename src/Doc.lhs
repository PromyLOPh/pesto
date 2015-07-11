Building documentation
++++++++++++++++++++++

.. class:: nodoc

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Pandoc
> import Text.Pandoc.Error (handleError)
> import Text.Highlighting.Kate.Styles (tango)
> import Data.List (stripPrefix)
> import System.FilePath (replaceFileName)
> import qualified Data.Set as S

The HTML documentation is generated directly from the source code of
Codec.Pesto. That module serves as starting point and it includes the other
modules in a sensible order. Pandoc_ renders the restructuredText_ to HTML. We
use a slightly modified template.

.. _pandoc: http://www.pandoc.org/
.. _restructuredText: http://docutils.sourceforge.net/rst.html

> main = do
> 	tpl <- readFile "template.html"
> 	doc <- readWithInclude "src/Codec/Pesto.lhs"
> 	writeFile "_build/index.html" $ rstToHtml tpl doc

Since pandoc currently does not support restructured text’s include directive
directly, emulate it by recursively replacing all lines starting with ``..
include::`` with the referenced file’s contents.

> readWithInclude f = do
> 	c <- readFile f
> 	let l = lines c
> 	linc <- mapM (\line -> case stripPrefix ".. include:: " line of
>		Just incfile -> readWithInclude $ replaceFileName f incfile
>		Nothing -> return line) l
> 	return $ unlines linc

The resulting string is parsed as literate Haskell with restructuredText markup.

> readDoc = readRST def {
> 	  readerExtensions = S.fromList [
> 		  Ext_literate_haskell
> 		, Ext_implicit_header_references
> 		]
> 	, readerStandalone = True }

Module definitions and imports should not be visible in the final
documentation. They are marked up with the class ``nodoc`` and removed from the
doctree before transforming it into HTML.

> dropNoDoc = topDown f
> 	where
> 		f (Div (_, classes, _) _) | "nodoc" `elem` classes = Null
> 		f x = x

> rstToHtml tpl = writeDoc tpl . dropNoDoc . handleError . readDoc

> writeDoc tpl = writeHtmlString def {
> 	  writerStandalone = True
> 	, writerTemplate = tpl
> 	, writerHtml5 = True
> 	, writerHighlight = True
> 	, writerHighlightStyle = tango
>	, writerNumberSections = True
> 	, writerSectionDivs = True
> 	, writerTabStop = 4
> 	, writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
> 	, writerVariables = [("css", "pesto.css"), ("lang", "en")]
> 	}

