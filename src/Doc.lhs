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

The documentation is created from the source module Codec.Pesto, which includes
the other modules. We use a slightly modified template.

> main = do
> 	tpl <- readFile "template.html"
> 	doc <- readWithInclude "src/Codec/Pesto.lhs"
> 	writeFile "_build/index.html" $ rstToHtml tpl doc

Since pandoc currently does not support restructured text’s include directive,
emulate it.

> readWithInclude f = do
> 	c <- readFile f
> 	let l = lines c
> 	linc <- mapM (\line -> case stripPrefix ".. include:: " line of
>		Just incfile -> readWithInclude $ replaceFileName f incfile
>		Nothing -> return line) l
> 	return $ unlines linc

The pass the resulting string to pandoc that builds a doctree, remove unwanted
content and build a HTML page.

> rstToHtml tpl = writeDoc tpl . dropNoDoc . handleError . readDoc

> readDoc = readRST def {
> 	  readerExtensions = S.fromList [
> 		  Ext_literate_haskell
> 		, Ext_implicit_header_references
> 		]
> 	, readerStandalone = True }

Drop blocks that should not be visible in the documentation, like module
definitions and imports.

> dropNoDoc = topDown f
> 	where
> 		f (Div (_, classes, _) _) | "nodoc" `elem` classes = Null
> 		f x = x

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

