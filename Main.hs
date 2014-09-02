{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 ((!), Html, a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
        [ dir "echo"    $ echo
        , dir "query"   $ queryParams
        , dir "form"    $ formPage
        , dir "fortune" $ fortune
        , dir "files"   $ fileServing
        , dir "upload"  $ upload
        , homePage
        ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage = ok $
  template "Home page" $ do
    H.h1 "Hello!"
    H.p "This is me, writing an app with Happstack lite."
    H.p "Here is stuff:"
    H.p $ a ! href "/echo/secret%20message" $ "echo"
    H.p $ a ! href "/query?foo=bar" $ "query parameters"
    H.p $ a ! href "/form" $ "form processing"
    H.p $ a ! href "/fortune" $ "(fortune) cookies"
    H.p $ a ! href "/files" $ "file serving"
    H.p $ a ! href "/upload" $ "file uploads "

echo :: ServerPart Response
echo = path $ \(msg :: String) ->
  ok $ template "echo" $ do
    p $ "echo says: " >> toHtml msg
    p "Change the URL to say something else."

queryParams :: ServerPart Response
queryParams =
  do mFoo <- optional $ lookText "foo"
     ok $ template "Query parameters" $ do
       p $ "foo is " >> toHtml (show mFoo)
       p $ "change the URL param to get something else."

formPage :: ServerPart Response
formPage = msum [viewForm, processForm]
  where
    viewForm :: ServerPart Response
    viewForm =
      do method GET
         ok $ template "form" $
           form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
             label ! A.for "msg" $ "Say something clever"
             input ! type_ "text" ! A.id "msg" ! name "msg"
             input ! type_ "submit" ! value "Say it!"
    processForm :: ServerPart Response
    processForm =
      do method POST
         msg <- lookText "msg"
         ok $ template "form" $ do
           H.p "You said:"
           H.p (toHtml msg)

fortune :: ServerPart Response
fortune = ok $ template "(Fortune) cookies" $ do H.h1 "(Fortune) cookies"

fileServing :: ServerPart Response
fileServing = ok $ template "File serving" $ do H.h1 "File serving"

upload :: ServerPart Response
upload = ok $ template "Uploads" $ do H.h1 "Uploads"
