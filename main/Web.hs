{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Gui (main) where

import RSTC.Car
import Interpreter.Golog
import RSTC.BAT.Regression
import RSTC.Progs
import RSTC.Theorems

import Control.Monad (msum)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


main :: IO ()
main = simpleHTTP nullConf myApp


myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


myApp :: ServerPart Response
myApp =
   do decodeBody myPolicy
      msum handlers

handlers :: [ServerPartT IO Response]
handlers = [ dir "signIn"  $ signIn
           , homePage
           ]


homePage :: ServerPart Response
homePage = msum [ signedIn, notSignedIn ]
   where signedIn =
            do ok $ template "Home page" $ do
               return () -- H.h1 $ toHtml $ "Hello!"
         notSignedIn =
            do seeOther ("/signIn" :: String) (toResponse ())


signIn :: ServerPart Response
signIn = msum [ alreadySignedIn, signInForm ]
    where
      alreadySignedIn :: ServerPart Response
      alreadySignedIn =
          do method GET
             ok $ template "Signed in" $ do
                    H.p $ "You are already signed in."

      signInForm :: ServerPart Response
      signInForm =
          do method GET
             ok $ template "Sign in" $ do
                    H.form ! A.action "/signIn" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
                    H.label ! A.for "MatriculationNumber" $ "Put your matriculation number: "
                    H.input ! A.type_ "text" ! A.id "MatriculationNumber" ! A.name "MatriculationNumberInput"
                    H.input ! A.type_ "submit" ! A.value "Sign in"


template :: Text -> Html -> Response
template title body = toResponse $
   H.html $ do
      H.head $ do
         H.title (toHtml title)
      H.body $ do
         body

