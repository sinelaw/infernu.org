--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import qualified Text.Pandoc as P

import           Text.Highlighting.Kate (pygments, styleToCss)
import Text.Pandoc.Error (handleError)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H

import           Infernu.Infer                (getAnnotations, minifyVars, runTypeInference)
import           Infernu.Parse                (translate)
import           Infernu.Types (GenInfo(..), Source(..))

import           Data.List                   (intersperse)
import           Data.Maybe                  (catMaybes)
import Text.PrettyPrint.ANSI.Leijen (plain, displayS, renderPretty, Doc, Pretty(..))
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3

showWidth :: Int -> Doc -> String
showWidth w x   = displayS (renderPretty 0.4 w x) ""
showDoc = showWidth 120
--------------------------------------------------------------------------------

static = do
    route   idRoute
    -- don't change its content
    compile copyFileCompiler

config = defaultConfiguration { destinationDirectory = "../s" }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "less/**" $
        compile getResourceBody

    match "bower_components/jquery/dist/**" $ static
    match "bower_components/bootstrap/dist/**" $ static
    -- match "node_modules/highlight.js/lib/**" $ static
    match "css/fonts/**" $ static

    create ["css/style.css"] $ do
        route idRoute
        compile $ makeItem (compressCss . styleToCss $ pygments)

    create ["css/main.css"] $ do
        route idRoute
        compile $ do
            getMatches "less/**"
            loadBody "less/all.less"
                >>= makeItem
                >>= withItemBody
                  (unixFilter "lessc" ["-"])

    match (fromList ["about.rst", "README.md"]) $ do -- , "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match (fromGlob "posts/*.md") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll (fromGlob "posts/*.md")
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls



    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll (fromGlob "posts/*.md")


            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    (constField "features" $ concatMap (\(t,d,c) -> featureItem t d c) features) `mappend`
                    (constField "tsFeatures" $ concatMap (\(t,d,c) -> featureItem t d c) typeSystemFeatures) `mappend`
--                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
--    dateField "date" "%B %e, %Y" `mappend`
--    constField "date" "2015-01-01" `mappend`
    defaultContext


markdownToHTML :: String -> H.Html
markdownToHTML =
  P.writeHtml P.def {P.writerReferenceLinks = True, P.writerHighlight = True, P.writerHighlightStyle = pygments, P.writerHtml5 = True } .
  handleError . P.readMarkdown P.def

--featureItem :: String -> String -> String -> H.Html
featureItem title description code =
    renderHtml
    $ H.div H.! H.customAttribute "class" "col-sm-6"
    $ do
        H.h1 $ H.string title
        H.p $ H.string description
        if code == ""
            then H.string ""
            else H.div H.! H.customAttribute "class" "well well-sm"
                 $ do
                     H.h5 $ H.string "Example:"
                     markdownToHTML ("```javascript\n" ++ code ++ "\n```")
                     H.h5 $ H.string "Inferred type:"
                     markdownToHTML ("```javascript\n" ++ (infernu code) ++ "\n```")

features =
    [("JavaScript Distilled",
      "JavaScript, without the awful parts. Runs as-is, no need for compilation or translation.",
      "")
    ,("Type Inference",
      "Static types without annotations. Just write your JavaScript, infernu will fill in the types.",
      "")]

typeSystemFeatures =
    [("Safety First",
      "Strong, expressive, static type system. No \"any\" type.",
      ""),
     ("Automatically Generic",
      "Infernu finds the most general type, and creates type parameters appropriately.",
      "function f(x) { return x; }"),
     ("Robust Structural Typing",
      "Row-type polymorphism keeps your code generic, while keeping track of the full type structure.",
      "function getLength(obj) { return obj.length; }"),
     ("Taming 'this'",
      "Rather than sweeping `this` under the rug, it's represented in the type system appropriately.",
      "function setX(x) { this.x = x; }")
     ]

infernu rest =
    case  runTypeInference . fmap Source . translate . ES3.unJavaScript <$> ES3Parser.parseFromString rest of
        Left e -> show e
        Right res ->
            case getAnnotations <$> res of
                Left e' -> showDoc $ pretty e'
                Right [] -> show "There is nothing there."
                Right xs -> concat . intersperse "\n" $ filterGen xs
                    where filterGen = catMaybes . map (\(Source (GenInfo g n, _), t) ->
                                                           case (g,n) of
                                                           (False, Just n) -> Just (showDoc (plain $ pretty n) ++ " : " ++ (showDoc $ plain $ pretty $ minifyVars t))
                                                           _ -> Nothing
                                                      )


