--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                     (mappend)
import           Hakyll

import           Control.Monad                   (forM_)
import qualified Text.Pandoc                     as P

import qualified Data.Text                       as Text
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import           Text.Highlighting.Kate          (pygments, styleToCss)
import           Text.Pandoc.Error               (handleError)

import           Infernu.Infer                   (getAnnotations, minifyVars, runTypeInference)
import           Infernu.Parse                   (translate)
import           Infernu.Types                   (GenInfo (..), Source (..))

import           Data.Char                       (toLower)
import           Data.List                       (intersperse)
import           Data.Maybe                      (catMaybes)
import qualified Language.ECMAScript3.Parser     as ES3Parser
import qualified Language.ECMAScript3.Syntax     as ES3
import           Text.PrettyPrint.ANSI.Leijen    (Doc, Pretty (..), displayS, plain, renderPretty)

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
                    (constField "features" $ concatMap (\ts -> renderHtml $ row $ map featureItem ts) $ chunks 2 features) `mappend`
                    (constField "tsFeatures" $ concatMap (\ts -> renderHtml $ row $ map featureItem ts) $ chunks 2 typeSystemFeatures) `mappend`
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

row es = H.div H.! H.customAttribute "class" "row"
         $ mconcat es
--featureItem :: String -> String -> String -> H.Html
featureItem (title, description, codes) =
    H.div H.! H.customAttribute "class" "col-sm-6"
    $ do
        H.h2 $ H.string title
        H.p $ markdownToHTML description
        let codesNum = length codes
        forM_ (zip [1..] codes) $ \(i, code) ->
            do H.div -- H.! H.customAttribute "class" "well well-sm"
               $ do
                   H.hr
                   H.h5 $ H.string $ "Example" ++ (if codesNum > 1 then " " ++ show i else "") ++ ":"
                   markdownToHTML ("```javascript\n" ++ code ++ "\n```")
                   H.h5 $ H.string "Infernu output:"
                   -- H.button (H.string "Infernu output")
                   --     H.! H.customAttribute "data-toggle" "collapse"
                   --     H.! H.customAttribute "data-target" "#collapseExample"
                   markdownToHTML ("```javascript\n" ++ (shortError . lines $ infernu code) ++ "\n```")
                       ---- H.! HA.id "collapseExample"
    where shortError [] = ""
          shortError [l] = l
          shortError (l:ls) = strip $ unlines $ l : truncError ls
          truncError ls = reverse . fst . break (\x -> (map toLower . strip $ x) == "because:") . reverse $ ls

features =
    [("JavaScript Distilled",
      "JavaScript, without the awful parts. Runs as-is, no need for compilation or translation.",
      [])
    ,("Type Inference",
      "Static types without annotations. Just write your JavaScript, infernu will fill in the types.",
      [])]

typeSystemFeatures =
    [("Safety First",
      "Strong, expressive, static type system. No \"any\" type.",
      ["var x = new Date();\nvar y = x + 1; // this does terrible things"]),
     ("Automatically Generic",
      "Infernu finds the most general type, and creates type parameters appropriately.",
      ["function f(x) { return x; }"]),
     ("Robust Structural Typing",
      "Row-type polymorphism keeps your code generic, while keeping track of the full type structure.",
      ["function getLength(obj) { return obj.length; }"]),
     ("Read-only Properties",
      "You can't override Math.PI, nor should you try. Infernu distinguishes - at the type level - between getting and setting a property.",
      ["Math.PI = 3.2;"]),
     ("Taming 'this'",
      "Rather than sweeping `this` under the rug, it's represented in the type system appropriately.",
      ["function setX(x) { this.x = x; }\n({ bla : setX, x : 0 }).bla(2); // OK! called with a valid 'this'",
       "function setX(x) { this.x = x; }\nsetX(2); // oops! setX will get 'undefined' for 'this'."]),
     ("Safe Overloading with Type Classes",
      "`a + b`: are they strings? numbers? Infernu handles such cases safely, without losing information.",
      ["function add(x,y) { return x + y; }\nvar a = add(1,2); var b = add('hello ', 'world');"]),
     ("Safe Indexing",
      "In `foo[x]`: What is `foo` - an array? A string? A map? Is `x` a number or a string? Infernu safely represents bracket syntax get/set.",
      ["function getAt(obj, ix) { return obj[ix]; }\nvar a = getAt([1,2], 0);\nvar b = getAt('hi', 2);"
      ,"var x = 'foo';\nx[0] = 't'; // oops... can't set string indexes!"])
    ]

infernu rest =
    case  runTypeInference . fmap Source . translate . ES3.unJavaScript <$> ES3Parser.parseFromString rest of
        Left e -> truncateLongErrors $ show e
        Right res ->
            case getAnnotations <$> res of
                Left e' ->  truncateLongErrors $ showDoc $ plain $ pretty e'
                Right [] -> show "There is nothing there."
                Right xs -> concat . intersperse "\n" $ filterGen xs
                    where filterGen = catMaybes . map (\(Source (GenInfo g n, _), t) ->
                                                           case (g,n) of
                                                           (False, Just n) -> Just (showDoc (plain $ pretty n) ++ " : " ++ (showDoc $ plain $ pretty $ minifyVars t))
                                                           _ -> Nothing
                                                      )


truncateLongErrors :: String -> String
truncateLongErrors x = unlines (take limit ls ++ suffix ++ afterLastBecause)
    where ls = lines x
          limit = max 0 $ min 1 (length ls - length afterLastBecause)
          afterLastBecause = take 6 $ reverse $ takeWhile (\x -> (map toLower $ strip x) /= "because:") $ reverse ls
          suffix = if length ls > limit
                   then ["(..." ++ (if length ls - limit > 10 then "many" else show $ length ls - limit) ++ " more lines)"]
                   else []

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack

chunks n l = case take n l of
                 [] -> []
                 l' -> l' : chunks n (drop n l)
