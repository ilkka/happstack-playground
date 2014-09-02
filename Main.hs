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
        [ dir "echo"    echo
        , dir "query"   queryParams
        , dir "form"    formPage
        , dir "fortune" fortune
        , dir "files"   fileServing
        , dir "upload"  upload
        , homePage
        ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $
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
       p "change the URL param to get something else."

formPage :: ServerPart Response
formPage = msum [viewForm, processForm]
  where
    viewForm :: ServerPart Response
    viewForm =
      do method GET
         ok $ template "form" $
           form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
             label ! A.for "msg" $ "Say something clever: "
             input ! type_ "text" ! A.id "msg" ! name "msg"
             input ! type_ "submit" ! value "Say it!"
    processForm :: ServerPart Response
    processForm =
      do method POST
         msg <- lookText "msg"
         ok $ template "form" $ do
           H.p "You said: "
           H.p (toHtml msg)

fortune :: ServerPart Response
fortune = msum [viewFortune, updateFortune]
  where
    viewFortune :: ServerPart Response
    viewFortune =
      do method GET
         mMemory <- optional $ lookCookieValue "fortune"
         let memory = fromMaybe "Your future is filled with web programming." mMemory
         ok $ template "Fortunes" $ do
           H.p "Your fortune sez: "
           H.p (toHtml memory)
           form ! action "/fortune" ! enctype "multipart/form-data" ! A.method "POST" $ do
             label ! A.for "fortune" $ "Change your fortune: "
             input ! type_ "text" ! A.id "fortune" ! name "new_fortune"
             input ! type_ "submit" ! value "Change it!"
    updateFortune :: ServerPart Response
    updateFortune =
      do method POST
         newFortune <- lookText "new_fortune"
         addCookies [(Session, mkCookie "fortune" (unpack newFortune))]
         seeOther ("/fortune" :: String) (toResponse ())

fileServing :: ServerPart Response
fileServing =
    serveDirectory EnableBrowsing ["index.html"] "."

upload :: ServerPart Response
upload = msum [uploadForm, handleUpload]
  where
    uploadForm :: ServerPart Response
    uploadForm =
      do method GET
         ok $ template "upload" $ do
           form ! action "/upload" ! enctype "multipart/form-data" ! A.method "POST" $ do
             label ! A.for "file" $ "Select file: "
             input ! type_ "file" ! A.id "file" ! name "file"
             input ! type_ "submit" ! value "Upload it!"
    handleUpload :: ServerPart Response
    handleUpload =
      do method POST
         (tmpFile, uploadName, contentType) <- lookFile "file"
         ok $ template "file uploaded" $ do
           H.p (toHtml $ "temporary file: " ++ tmpFile)
           H.p (toHtml $ "uploaded name: " ++ uploadName)
           H.p (toHtml $ "content type: " ++ show contentType)

