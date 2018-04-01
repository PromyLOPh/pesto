Building documentation
++++++++++++++++++++++

.. class:: nodoc

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Pandoc
> import Text.Pandoc.Error (handleError)
> import Text.Pandoc.Extensions (extensionsFromList)
> import Text.Pandoc.Highlighting (tango)
> import qualified Data.Text.IO as TIO
> import System.Directory (setCurrentDirectory)

The documentation can be generated running ``cabal run pesto-doc``. It is
exclusively based on the restructuredText inside this packages’ literal Haskell
source code.

.. _restructuredText: http://docutils.sourceforge.net/rst.html

> readDoc = readRST def {
> 	  readerExtensions = extensionsFromList [
> 		  Ext_literate_haskell
> 		, Ext_implicit_header_references
> 		]
> 	, readerStandalone = True }

.. _Pandoc: http://www.pandoc.org/

Pandoc_ outputs a single HTML5 page with syntax highlighting and MathJax for
formulas.

> writeDoc tpl = writeHtml5String def {
> 	  writerTemplate = Just tpl
> 	, writerHighlightStyle = Just tango
>	, writerNumberSections = True
> 	, writerSectionDivs = True
> 	, writerTabStop = 4
> 	, writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
> 	, writerVariables = [("css", "pesto.css"), ("lang", "en")]
> 	}

A slightly customized template is used.

> main = do
> 	tpl <- readFile "template.html"

The module Codec.Pesto serves as starting point and it includes every other
module in a sensible order. For the relative includes to work, we need to
change our current working directory.

>	setCurrentDirectory "src/Codec"
> 	doc <- TIO.readFile "Pesto.lhs"
> 	result <- runIO $ readDoc doc >>= writeDoc tpl
>	setCurrentDirectory "../../"
> 	html <- handleError result

Output is written to the directory ``_build``, which contains the corresponding
stylesheet.

> 	TIO.writeFile "_build/index.html" html

