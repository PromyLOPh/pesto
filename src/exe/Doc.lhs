Building documentation
++++++++++++++++++++++

.. class:: nodoc

> {-# LANGUAGE OverloadedStrings #-}
> import Text.Pandoc
> import Text.Pandoc.Highlighting (tango)
> import qualified Data.Text.IO as TIO
> import System.Directory (setCurrentDirectory)
> import Data.Map as M
> import Text.DocTemplates (ToContext(toVal), Context(..))
> import Data.Text (pack)
> import Data.Either.Combinators (rightToMaybe)

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
>	  writerTemplate = tpl
> 	, writerHighlightStyle = Just tango
>	, writerNumberSections = True
> 	, writerSectionDivs = True
> 	, writerTabStop = 4
> 	, writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
> 	, writerVariables = Context $ M.fromList [
> 		  ("css", toVal $ pack "pesto.css")
> 		, ("lang", toVal $ pack "en")
> 		, ("include-before", toVal $ pack "<div class=\"wrapper\">")
> 		, ("include-after", toVal $ pack "</div>")
> 		]
> 	}
>
> main = do

The module Codec.Pesto serves as starting point and it includes every other
module in a sensible order. For the relative includes to work, we need to
change our current working directory.

>	tpl <- runIO $ compileDefaultTemplate "html5"
>	setCurrentDirectory "src/lib/Codec"
> 	doc <- TIO.readFile "Pesto.lhs"
> 	result <- runIO $ readDoc doc >>= writeDoc (rightToMaybe tpl)
>	setCurrentDirectory "../../../"
> 	html <- handleError result

Output is written to the directory ``_build``, which contains the corresponding
stylesheet.

> 	TIO.writeFile "_build/index.html" html

